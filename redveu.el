(require 'elmine)
(require 's)

(setq redveu/identity "IDENTITY");

(setq redveu/statuses (make-hash-table :test 'equal))
(setq redveu/trackers (make-hash-table :test 'equal))
(setq redveu/priorities (make-hash-table :test 'equal))
(setq redveu/users (make-hash-table :size 100 :test 'equal))

(setq redveu/queries (make-hash-table :test 'equal))
(setq redveu/versions (make-hash-table :test 'equal))

(setq redveu/veupathdb-team-property nil)

(setq redveu/manager-concern-property nil)
(setq redveu/pip-property nil)

(defun redveu/makeHash(objs hash)
  (when objs
    (setq o (car objs))
    (setq name (elmine/get o :name))
    (puthash name (elmine/get o :id) hash)
    (redveu/makeHash(cdr objs) hash)
    )
  )


(defun redveu/makeUsersHash(objs hash)
  (when objs
    (setq m (car objs))
    (setq o (elmine/get m :user))
    (if o
	(progn 
	  (setq name (elmine/get o :name))
	  (puthash name (elmine/get o :id) hash)
	  )
      )
    (redveu/makeUsersHash(cdr objs) hash)
    )
  )




(defun redveu/get-projects (query_id &optional arg)
  "Get projects using optional query"
  (interactive (list (read-string "Query Id (Required): ") current-prefix-arg))
   (if query_id 
       (progn (redveu/init query_id "PROJECT")
	      (redveu/parse-projects-to-org 
	       (elmine/get-projects :query_id query_id)
	       )
	      )
     )
   (org-set-startup-visibility)
)



(defun redveu/update-version (val &optional arg)
  (interactive
   (list
    (completing-read "Choose one: " (if (> (hash-table-count redveu/versions)  0)
					(hash-table-keys redveu/versions)
				      (progn
					;; TODO: The project name here is hard coded (kjowxz).  
					(redveu/makeHash (elmine/api-get-all :versions "/projects/kjowxz/versions.json" :limit 100) redveu/versions)
					(hash-table-keys redveu/versions)
					)
				      ) current-prefix-arg)))

  (setq id (gethash val redveu/versions))
  (if id
      (redveu/update-issue-property :fixed_version_id id)
    (error "Not a valid selection")
    )
  )




(defun redveu/update-tracker (val &optional arg)
  (interactive
   (list
    (completing-read "Choose one: " (if (> (hash-table-count redveu/trackers)  0)
					(hash-table-keys redveu/trackers)
				      (progn
					(redveu/makeHash (elmine/get-trackers) redveu/trackers)
					(hash-table-keys redveu/trackers)
					)
				      ) current-prefix-arg)))

  (setq id (gethash val redveu/trackers))
  (if id
      (redveu/update-issue-property :tracker_id id)
    (error "Not a valid selection")
    )
  )

(defun redveu/update-priority (val &optional arg)
  (interactive
   (list
    (completing-read "Choose one: " (if (> (hash-table-count redveu/priorities)  0)
					(hash-table-keys redveu/priorities)
				      (progn
					(redveu/makeHash (elmine/get-issue-priorities) redveu/priorities)
					(hash-table-keys redveu/priorities)
					)
				      ) current-prefix-arg)))

  (setq id (gethash val redveu/priorities))
  (if id
      (redveu/update-issue-property :priority_id id)
    (error "Not a valid selection")
    )
  )

(defun redveu/update-status (val &optional arg)
  (interactive
   (list
    (completing-read "Choose one: " (if (> (hash-table-count redveu/statuses)  0)
					(hash-table-keys redveu/statuses)
				      (progn
					(redveu/makeHash (elmine/get-issue-statuses) redveu/statuses)
					(hash-table-keys redveu/statuses)
					)
				      ) current-prefix-arg)))

  (setq id (gethash val redveu/statuses))
  (if id
      (redveu/update-issue-property :status_id id)
    (error "Not a valid selection")
    )
  )

(defun redveu/update-assigned-to (val &optional arg)
  (interactive
   (list
    (completing-read "Choose one: " (if (> (hash-table-count redveu/users)  0)
					(hash-table-keys redveu/users)
				      (progn
					;; TODO: The project name here is hard coded (kjowxz).  Each project has a list of member/users.  Could look up theh users for each project in that way.  
					(redveu/makeUsersHash (elmine/api-get-all :memberships "/projects/kjowxz/memberships.json" :limit 100) redveu/users)
					(hash-table-keys redveu/users)
					)
				      ) current-prefix-arg)))

  (setq id (gethash val redveu/users))
  (if id
      (redveu/update-issue-property :assigned_to_id id)
    (error "Not a valid selection")
    )
  )

;; only doing this for VEuPath Team currently...could add more as needed
(defun redveu/set-custom-properties()
  (setq customFields (elmine/api-get-all :custom_fields "/custom_fields.json"))
  (while customFields
    (setq field (car customFields))
    (if (string= (elmine/get field :name) "VEuPathDB Team")
	(setq redveu/veupathdb-team-property field)
      )

    (if (string= (elmine/get field :name) "Manager concern")
	(setq redveu/manager-concern-property field)
      )

    (if (string= (elmine/get field :name) "PIP")
	(setq redveu/pip-property field)
      )
    
    (setq customFields (cdr customFields))
    )
  )



(defun redveu/update-veupathdb-team (val &optional arg)
  (interactive
   (list
    (completing-read "Choose one: " (if redveu/veupathdb-team-property
					(mapcar #'(lambda(x) (elmine/get x :value)) (elmine/get redveu/veupathdb-team-property :possible_values))
					;;(elmine/get redveu/veupathdb-team-property :possible_values)
				      (progn
					(redveu/set-custom-properties)
					(mapcar #'(lambda(x) (elmine/get x :value)) (elmine/get redveu/veupathdb-team-property :possible_values))
					;;(elmine/get redveu/veupathdb-team-property :possible_values)
					)
				      ) current-prefix-arg)))

  (setq id (elmine/get redveu/veupathdb-team-property :id))
  (if id
      (redveu/update-issue-custom-property id val)
    (error "Problem finding id for EuPathDB Team Custom Property")
    )
  )


(defun redveu/update-manager-concern (val &optional arg)
  (interactive
   (list
    (completing-read "Choose one: " (if redveu/manager-concern-property
					(mapcar #'(lambda(x) (elmine/get x :value)) (elmine/get redveu/manager-concern-property :possible_values))
				      (progn
					(redveu/set-custom-properties)
					(mapcar #'(lambda(x) (elmine/get x :value)) (elmine/get redveu/manager-concern-property :possible_values))
					)
				      ) current-prefix-arg)))

  (setq id (elmine/get redveu/manager-concern-property :id))
  (if id
      (redveu/update-issue-custom-property id val)
    (error "Problem finding id for EuPathDB Team Custom Property")
    )
  )


(defun redveu/update-pip (val &optional arg)
  (interactive
   (list
    (completing-read "Choose one: " (if redveu/pip-property
					(mapcar #'(lambda(x) (elmine/get x :value)) (elmine/get redveu/pip-property :possible_values))
				      (progn
					(redveu/set-custom-properties)
					(mapcar #'(lambda(x) (elmine/get x :value)) (elmine/get redveu/pip-property :possible_values))
					)
				      ) current-prefix-arg)))

  (setq id (elmine/get redveu/pip-property :id))
  (if id
      (redveu/update-issue-custom-property id val)
    (error "Problem finding id for EuPathDB Team Custom Property")
    )
  )




(defun redveu/update-subject (val &optional arg)
  (interactive (list (read-string "Enter New Subject: ") current-prefix-arg))
  (if val
      (redveu/update-issue-property :subject val)
    (error "Subject Not Provided")
    )
  )

(defun redveu/add-comment (val &optional arg)
  (interactive (list (read-string "Comment: ") current-prefix-arg))
  (if val
      (redveu/update-issue-property :notes val)
    (error "Comment Not Provided")
    )
  )



(defun redveu/create-issue (priority tracker assignedTo veupathTeam version subject description &optional arg)
  (interactive
   (list
    (completing-read "Priority: " (if (> (hash-table-count redveu/priorities)  0)
				      (hash-table-keys redveu/priorities)
				    (progn
				      (redveu/makeHash (elmine/get-issue-priorities) redveu/priorities)
				      (hash-table-keys redveu/priorities)
				      )
				    )
		     nil nil "Normal"
		     )
    (completing-read "Tracker: " (if (> (hash-table-count redveu/trackers)  0)
					(hash-table-keys redveu/trackers)
				      (progn
					(redveu/makeHash (elmine/get-trackers) redveu/trackers)
					(hash-table-keys redveu/trackers)
					)
				      )
		     nil nil "Task"
		     )

    (completing-read "Assigned To: " (if (> (hash-table-count redveu/users)  0)
					(cons "None" (hash-table-keys redveu/users))
				      (progn
					;; TODO: The project name here is hard coded (kjowxz).  Each project has a list of member/users.  Could look up theh users for each project in that way.  
					(redveu/makeUsersHash (elmine/api-get-all :memberships "/projects/kjowxz/memberships.json" :limit 100) redveu/users)
					(cons "None" (hash-table-keys redveu/users))
					)
				      )
		     nil nil "None"
		     )

    (completing-read "VEuPath Team: " (if redveu/veupathdb-team-property
					(mapcar #'(lambda(x) (elmine/get x :value)) (elmine/get redveu/veupathdb-team-property :possible_values))
				      (progn
					(redveu/set-custom-properties)
					(mapcar #'(lambda(x) (elmine/get x :value)) (elmine/get redveu/veupathdb-team-property :possible_values))
					)
				      )
		     nil nil "Data Development"
		     )
    (completing-read "Milestone: " (if (> (hash-table-count redveu/versions)  0)
					(cons "None" (hash-table-keys redveu/versions))
				      (progn
					;; TODO: The project name here is hard coded (kjowxz).  
					(redveu/makeHash (elmine/api-get-all :versions "/projects/kjowxz/versions.json" :limit 100) redveu/versions)
					(cons "None" (hash-table-keys redveu/versions))
					)
				      )
		     nil nil "None"
		     )
    (read-string "Subject: ")
    (read-string "Description: ")
    current-prefix-arg
    )
   )

  (setq projectId (org-entry-get (point) "project_id"))
  (if projectId
      (progn 
	(setq issue '())
	(setq issue (plist-put issue :project_id projectId))
	(setq issue (plist-put issue :subject subject))
	(setq issue (plist-put issue :description description))
	(setq issue (plist-put issue :tracker_id (gethash tracker redveu/trackers)))

	(if (string= version "None")
	    nil
	    (setq issue (plist-put issue :fixed_version_id (gethash version redveu/versions)))
	  )
	
	(if (string= assignedTo "None")
	    nil
	    (setq issue (plist-put issue :assigned_to_id (gethash assignedTo redveu/users)))
	  )
	(setq issue (plist-put issue :priority_id (gethash priority redveu/priorities)))
 	(setq customField '())
	(setq customField (plist-put customField :id (elmine/get redveu/veupathdb-team-property :id)))
	(setq customField (plist-put customField :value veupathTeam))
	(setq customFields (vector customField))
	(setq issue (plist-put issue :custom_fields customFields))
	(elmine/create-issue issue)
	)
    (error "Could not find a property of project_id")
    )
  )



(defun redveu/create-subtask (priority tracker assignedTo veupathTeam version subject description &optional arg)
  (interactive
   (list
    (completing-read "Priority: " (if (> (hash-table-count redveu/priorities)  0)
				      (hash-table-keys redveu/priorities)
				    (progn
				      (redveu/makeHash (elmine/get-issue-priorities) redveu/priorities)
				      (hash-table-keys redveu/priorities)
				      )
				    )
		     nil nil "Normal"
		     )
    (completing-read "Tracker: " (if (> (hash-table-count redveu/trackers)  0)
					(hash-table-keys redveu/trackers)
				      (progn
					(redveu/makeHash (elmine/get-trackers) redveu/trackers)
					(hash-table-keys redveu/trackers)
					)
				      )
		     nil nil "Task"
		     )

    (completing-read "Assigned To: " (if (> (hash-table-count redveu/users)  0)
					(cons "None" (hash-table-keys redveu/users))
				      (progn
					;; TODO: The project name here is hard coded (kjowxz).  Each project has a list of member/users.  Could look up theh users for each project in that way.  
					(redveu/makeUsersHash (elmine/api-get-all :memberships "/projects/kjowxz/memberships.json" :limit 100) redveu/users)
					(cons "None" (hash-table-keys redveu/users))
					)
				      )
		     nil nil "None"
		     )

    (completing-read "VEuPath Team: " (if redveu/veupathdb-team-property
					(mapcar #'(lambda(x) (elmine/get x :value)) (elmine/get redveu/veupathdb-team-property :possible_values))
				      (progn
					(redveu/set-custom-properties)
					(mapcar #'(lambda(x) (elmine/get x :value)) (elmine/get redveu/veupathdb-team-property :possible_values))
					)
				      )
		     nil nil "Data Development"
		     )
    (completing-read "Milestone: " (if (> (hash-table-count redveu/versions)  0)
					(cons "None" (hash-table-keys redveu/versions))
				      (progn
					;; TODO: The project name here is hard coded (kjowxz).  
					(redveu/makeHash (elmine/api-get-all :versions "/projects/kjowxz/versions.json" :limit 100) redveu/versions)
					(cons "None" (hash-table-keys redveu/versions))
					)
				      )
		     nil nil "None"
		     )
    (read-string "Subject: ")
    (read-string "Description: ")
    current-prefix-arg
    )
   )

  (setq projectId (org-entry-get (point) "project_id"))
  (setq issueId (org-entry-get (point) "issue_id"))  
  (if projectId
      (progn 
	(setq issue '())
	(setq issue (plist-put issue :project_id projectId))
	(setq issue (plist-put issue :parent_issue_id issueId))
	(setq issue (plist-put issue :subject subject))
	(setq issue (plist-put issue :description description))
	(setq issue (plist-put issue :tracker_id (gethash tracker redveu/trackers)))

	(if (string= version "None")
	    nil
	    (setq issue (plist-put issue :fixed_version_id (gethash version redveu/versions)))
	  )
	
	(if (string= assignedTo "None")
	    nil
	    (setq issue (plist-put issue :assigned_to_id (gethash assignedTo redveu/users)))
	  )
	(setq issue (plist-put issue :priority_id (gethash priority redveu/priorities)))
 	(setq customField '())
	(setq customField (plist-put customField :id (elmine/get redveu/veupathdb-team-property :id)))
	(setq customField (plist-put customField :value veupathTeam))
	(setq customFields (vector customField))
	(setq issue (plist-put issue :custom_fields customFields))
	(elmine/create-issue issue)
	)
    (error "Could not find a property of project_id")
    )
  )


(defun redveu/update-issue-property (element propertyId)
  (setq issueId (org-entry-get (point) "issue_id"))
  (if issueId
      (progn 
	(setq issue '())
	(setq issue (plist-put issue :id issueId))
	(setq issue (plist-put issue element propertyId))
	(elmine/update-issue issue)
	(redveu/refresh-issue issueId)
	)
    (error "Could not find a property of issue_id")
    )
  )

(defun redveu/update-issue-custom-property (propId propValue)
  (setq issueId (org-entry-get (point) "issue_id"))
  (if issueId
      (progn 
	(setq issue '())
	(setq issue (plist-put issue :id issueId))

	(setq customField '())
	(setq customField (plist-put customField :id propId))
	(setq customField (plist-put customField :value propValue))
	(setq customFields (vector customField))

	(setq issue (plist-put issue :custom_fields customFields))
	(elmine/update-issue issue)
	(redveu/refresh-issue issueId)
	)
    (error "Could not find a custom custom property")
    )
  )


(defun redveu/parse-projects-to-org(projects)
  (when projects
    (redveu/parse-project-to-org(car projects))
    (redveu/parse-projects-to-org(cdr projects))
    )
  )

(defun redveu/parse-project-to-org(p)
  (setq projectIdentifier (elmine/get p :identifier))
  (setq projectName (elmine/get p :name))
  (setq projectDescription (elmine/get p :description))

  (org-insert-heading)

  (redveu/goto-level 2)

  (insert projectName "\n" )
  
  (org-cycle)
;;  (org-insert-link nil (concat elmine/host "/projects/" projectIdentifier) "Project")
  (insert (concat "[[" (concat elmine/host "/projects/" projectIdentifier) "][Project]]\n"))

  (org-cycle)
;;  (org-insert-link nil (concat elmine/host "/projects/" projectIdentifier "/issues/new") "New Issue")   
  (insert (concat "[[" (concat elmine/host "/projects/" projectIdentifier "/issues/new") "][New Issue]]\n"))

  (org-insert-property-drawer)
  (org-set-property "project_identifier" projectIdentifier)
  (org-set-property "project_name" projectName)
  (org-set-property "project_id" (format "%s" (elmine/get p :id)))
  
  (if projectDescription
      (progn
	(org-insert-heading)
	(redveu/goto-level 3)
	(insert "COMMENT Description" "\n")
	(insert "#+BEGIN_DESCRIPTION" "\n")
	(insert (redveu/clean-description projectDescription) "\n")
	(insert "#+END_DESCRIPTION" "\n")
	)
    )
  )

(defun redveu/goto-level(l)
  (while (/= (org-outline-level) l)
    (if (> (org-outline-level) l)
	(org-promote)
      )
    (if (< (org-outline-level) l)
	(org-demote)
      )
    )
  )

(defun redveu/init(q type)
;;  (erase-buffer)
  (end-of-buffer)
  (org-insert-heading)
  (redveu/goto-level 1)

  (insert type " QUERY " (format "%s" q) " ")
  (org-insert-time-stamp (current-time))
  (insert "\n")
  (org-insert-property-drawer)
  (org-set-property "query_id" (format "%s" q))
  (org-set-property "query_type" type)
  (if (string= type "ISSUE")
      (org-set-property "COLUMNS" "%priorityZ %tracker %author_assigned %VEuPathDB_Team %40subject %30project_name %version")
      )
  )


;; this is not used
;;(defun redveu/projects-from-issues(issues)
;;   (mapcar #'(lambda(x) (elmine/get (elmine/get x :project) :name))  issues)
;;)

(defun redveu/get-issues-by-id (query_id &optional arg)
  "Get issues using query"
  (interactive (list (read-string "Query Id (Required): ") current-prefix-arg))
   (if query_id 
       (progn (redveu/init query_id "ISSUE")
	      (redveu/parse-issues-to-org 
	       (elmine/get-issues :query_id query_id)
	       )
	      )
     )
   (org-set-startup-visibility)
  )


(defun redveu/get-issues (val &optional arg)
  (interactive
   (list
    (completing-read "Choose one: " (if (> (hash-table-count redveu/queries)  0)
					(hash-table-keys redveu/queries)
				      (progn
					(redveu/makeHash (elmine/api-get-all :easy_queries "/queries.json") redveu/queries)
					(hash-table-keys redveu/queries)
					)
				      ) current-prefix-arg)))

  (setq query_id (gethash val redveu/queries))
  (if query_id
      (progn (redveu/init query_id "ISSUE")
	     (redveu/parse-issues-to-org 
	      (elmine/get-issues :query_id query_id)
	      )
	     )
    (error "Not a valid selection")
    )
  (org-set-startup-visibility)
  )

(defun redveu/get-project-issues (query_id &optional arg)
  "Get issues using query"
  (interactive (list (read-string "Query Id (Required): ") current-prefix-arg))
  (setq pn (org-entry-get (point) "project_name"))
  (setq pid (org-entry-get (point) "project_id"))
  (setq m (concat pn " ISSUES"))
  (if query_id 
      (if pid
	  (progn (redveu/init query_id m)
		 (redveu/parse-issues-to-org 
		  (elmine/get-project-issues (string-to-number pid) :query_id query_id)
		  )
		 )
	  )
    )
   (org-set-startup-visibility)
  )

(defun redveu/parse-issues-to-org(issues)
  (when issues
    (redveu/parse-issue-to-org(car issues))
    (redveu/parse-issues-to-org(cdr issues))
    )
  )

(defun redveu/parse-custom-fields-to-property(customFields)
  (when customFields
    (redveu/parse-custom-field-to-property(car customFields))
    (redveu/parse-custom-fields-to-property(cdr customFields))
    )
  )

(defun redveu/parse-custom-field-to-property(customField)
  (setq prop (elmine/get customField :name))

  ;;(setq value "NA")
  (if (char-or-string-p (elmine/get customField :value))
      (setq value (elmine/get customField :value ))
    )
  (if (string= value "")
      (setq value "NA")
    )

  (if (string= prop "VEuPathDB Team")
      (org-set-property "VEuPathDB_Team" value)
    )

  (if (string= prop "PIP")
      (org-set-property "PIP" value)
    )

  (if  (string= prop "Manager concern")
      (org-set-property "Manager_concern" value)
    )
  )


(defun redveu/group-issues (prop arg)
  (interactive
   (list
    (completing-read "Choose Property: " '("priority" "status" "tracker" "assigned_to" "author" "project_name" "PIP" "Manager_concern" "version" "VEuPathDB_Team")
				      ) current-prefix-arg))

  (setq queryId (org-entry-get (point) "query_id"))
  (if queryId 
      (progn (org-sort-entries nil ?r nil nil prop)
	     (outline-next-visible-heading 1)
	     (setq prevPropVal nil)
	     (setq nextIssueId (org-entry-get (point) "issue_id"))
	     (while nextIssueId
	       (setq propValue (org-entry-get (point) prop))
	       (unless propValue (setq propValue "None"))
	       (setq issueId (org-entry-get (point) "issue_id"))
	       (unless (string= prevPropVal propValue)
		   (progn
		     (org-insert-heading)
		     (redveu/goto-level 3)
		     (insert propValue)
		     (org-move-subtree-up)
		     (redveu/goto-level 2)
		     (setq prevPropVal propValue)
		     (outline-next-visible-heading 1)
		     )
		   )
	       (ignore-errors
		 (outline-forward-same-level 1)
		 )
	       (setq nextIssueId (org-entry-get (point) "issue_id"))
	       (if (string=  nextIssueId issueId)
		   (setq nextIssueId nil)		   
		   )
		   
	       )
	     )
    )
    (org-set-startup-visibility)
  )



(defun redveu/refresh-query(arg)
  "refresh previously run query"
  (interactive "P")
  (setq queryId (org-entry-get (point) "query_id"))
  (setq queryType (org-entry-get (point) "query_type"))


  (org-cut-subtree)
  (previous-line)

  (redveu/init queryId queryType)
  
  (if (string= queryType "ISSUE")
      (progn (redveu/parse-issues-to-org 
	      (elmine/get-issues :query_id queryId)
	      )
	     )
      )

  (if (string= queryType "PROJECT")
      (progn (redveu/parse-projects-to-org 
	      (elmine/get-projects :query_id queryId)
	      )
	     )
      )
  (org-set-startup-visibility)
  )



(defun redveu/refresh-issue(issueId)
  (org-cut-subtree)
  (previous-line)
  (setq issue (elmine/get-issue (string-to-number issueId)  :include "journals,attachments"))
  (redveu/parse-issue-to-org issue)
  (redveu/parse-journals-to-org (elmine/get issue :journals))
  (redveu/parse-attachments-to-org (elmine/get issue :attachments))

  ;; do this to get back to the issue part instead of descriptions/comments
  (outline-up-heading 1)
  )

(defun redveu/parse-issue-to-org(i)
  (setq projectName (elmine/get (elmine/get i :project) :name))
  (setq projectId (elmine/get (elmine/get i :project) :id))

  (setq issueId (elmine/get i :id))
  (setq status (elmine/get (elmine/get i :status) :name))
  (setq priority (elmine/get (elmine/get i :priority) :name))
  (setq tracker (elmine/get (elmine/get i :tracker) :name))

  (setq assignedTo (elmine/get (elmine/get i :assigned_to) :name))
  (setq author (elmine/get (elmine/get i :author) :name))

  (setq category (elmine/get (elmine/get i :category) :name))

  (setq fixedVersion (elmine/get (elmine/get i :fixed_version) :name))
  
  (org-insert-heading)
  (redveu/goto-level 3)

  (setq assignedToAbbrev "")
  (setq assignedToAbbrev2 "Nobody")
  (setq todoTag "THEM")
  (if assignedTo
      (progn 
	(setq assignedToList (split-string assignedTo))
	(setq firstName (pop assignedToList))
	(setq assignedToAbbrev (concat (substring firstName 0 1) (substring (car assignedToList) 0 1)))
	(setq assignedToAbbrev2 (concat firstName (substring (car assignedToList) 0 1)))
	(if (string= redveu/identity assignedTo)
	    (setq todoTag "TODO")
	  )
	)
    )

  (if (string= assignedToAbbrev2 "Nobody")
      (setq todoTag "ASSIGN")
    )

  (setq authorAbbrev "NA")
  (if author
      (progn
	(setq authorList (split-string author ))
	(setq authorFirstName (pop authorList))
	(if authorList
	    (setq authorAbbrev (concat (substring authorFirstName 0 1) (substring (car authorList) 0 1)))
	  (setq authorAbbrev authorFirstName)
	  )
	)
    )

  (setq subject (elmine/get i :subject))
  
  (setq displayCategory "None")
  (if category
      (setq displayCategory category)
    )

  
  (insert
   (format "%-80s %-26s %-10s"
;;   		  (concat authorAbbrev "->" assignedToAbbrev)
;;		  (substring projectName 0 (min 20 (length projectName)))
	   (concat todoTag " " (substring subject 0 (min 70 (length subject))) "..." )
	   (concat " /" (substring projectName 0 (min 20 (length projectName))) ".../")
           (concat  ":" assignedToAbbrev2 ":")
   		  )
   	  )

  (insert "\n\n")
  (org-cycle)
  (insert (concat "[[" (concat elmine/host "/issues/" (number-to-string issueId)) "][Issue Page]]\n"))
  
  (org-cycle)
  (insert (concat "[[" (concat elmine/host "/projects/" (number-to-string projectId) "/issues/new?subtask_for_id=" (number-to-string issueId)) "][Add Subtask]]\n"))

  (org-cycle)
  (insert (concat "[[" (concat elmine/host "/projects/" (number-to-string projectId) "/issues/new") "][New Issue (" projectName ")]]\n"))
  
  (org-insert-property-drawer)

  (org-set-property "issue_id" (format "%s" issueId))
  (org-set-property "subject" (concat "*" subject "*"))
  (org-set-property "status" status)
  (org-set-property "priorityZ" priority)
  (org-set-property "tracker" tracker)
  (org-set-property "author" author)
  (if assignedTo
      (org-set-property "assigned_to" assignedTo)
    )
;;  (org-set-property "author_assigned" (concat authorAbbrev "->" assignedToAbbrev))
  (org-set-property "project_id" (format "%s" projectId))
  (org-set-property "project_name" projectName)

  (if fixedVersion
      (org-set-property "version" fixedVersion)
    )
  (redveu/parse-custom-fields-to-property (elmine/get i :custom_fields))

  (org-set-property "checkedForComments" "no")
  
  (org-insert-heading)
  (redveu/goto-level 4)
  (insert "COMMENT Description" "\n")
  (insert "#+BEGIN_DESCRIPTION" "\n")
  (insert (redveu/clean-description (elmine/get i :description)) "\n")
  (insert "#+END_DESCRIPTION" "\n")
  )

;; could this be done automatically when user opens an issue section?
(defun redveu/verbose-issue(arg)
  "Get issues using query"
  (interactive "P")

  (setq issueId (org-entry-get (point) "issue_id"))
  (if issueId
      (redveu/add-journals-and-attachments issueId)
    )
  )

(defun redveu/add-journals-and-attachments(issueId)
  (setq i (elmine/get-issue issueId :include "journals,attachments"))

  (setq journals (elmine/get i :journals))
  (setq attachments (elmine/get i :attachments))

  (redveu/parse-journals-to-org journals)
  (redveu/parse-attachments-to-org attachments)

  (setq noteCount 0)
  (while journals
    (setq journal (car journals))
    (if (/= 0 (length (elmine/get journal :notes)))
	(setq noteCount (+ noteCount 1))
      )
    (setq journals (cdr journals))
    )

  (+ (length attachments) noteCount)
  )


(defun redveu/parse-attachments-to-org(attachments)
  (when attachments
    (redveu/parse-attachment-to-org(car attachments))
    (redveu/parse-attachments-to-org(cdr attachments))
    )
  )

(defun redveu/parse-attachment-to-org(a)
  (org-insert-heading)
  (redveu/goto-level 4)
  (insert "Attachment ")
;;  (org-insert-link nil (elmine/get a :content_url) (elmine/get a :filename))
  (insert (concat "[[" (elmine/get a :content_url) "][" (elmine/get a :filename) "]]"))
  (insert " by " (elmine/get (elmine/get a :author) :name) " on " (elmine/get a :created_on) "\n")
  )

(defun redveu/parse-journals-to-org(journals)
  (when journals
    (redveu/parse-journal-to-org(car journals))
    (redveu/parse-journals-to-org(cdr journals))
    )
  )

(defun redveu/parse-journal-to-org(j)
  (if (/= 0 (length (elmine/get j :notes))) 
      (progn
	(org-insert-heading)
	(redveu/goto-level 4)
	(insert "COMMENT " (elmine/get (elmine/get j :user) :name) " on " (elmine/get j :created_on) "\n")
	(insert "#+BEGIN_COMMENT" "\n")
	(insert (redveu/clean-description (elmine/get j :notes)) "\n")
	(insert "#+END_COMMENT" "\n")
	(outline-hide-subtree)
	)
    )
  )

(defun redveu/clean-description(d)
  (if d
      (replace-regexp-in-string "*" " *"
				(with-temp-buffer
				  (insert d)
				  (shr-render-region (point-min) (point-max))
				  (buffer-substring-no-properties (point-min) (point-max)))
				)
    )
  )

(defsubst hash-table-keys (hash-table)
  "Return a list of keys in HASH-TABLE."
  (let ((keys '()))
    (maphash (lambda (k _v) (push k keys)) hash-table)
    keys))

(defun redveu/org-cycle(arg)
  (interactive "P")

  (setq issueId (org-entry-get (point) "issue_id"))
  (setq checkedForComments (org-entry-get (point) "checkedForComments"))
  (if (and issueId (string= checkedForComments "no"))
      (progn
	;; Exit out of column mode if we are in it
	(if (fboundp 'org-columns-quit)
	    (org-columns-quit) 
	  )
	(org-set-property "checkedForComments" "done")
	(if (> (redveu/add-journals-and-attachments issueId) 0)
	    (outline-up-heading 1)
	  )
	)
    )
  (org-cycle)
  )

(add-hook 'org-mode-hook
       (lambda ()
	 (local-set-key (kbd "<tab>") 'redveu/org-cycle)
	 ))


;; (defun redveu/clear-subtree ()
;;   (interactive)
;;   (org-mark-subtree) ;; mark the current subtree
;;   (forward-line) ;; move point forward, so the headline isn't in the region
;;   (delete-region (region-beginning) (region-end)) ;; delete the rest
;; )

;; will remove 
;; (org-cut-subtree)


;; get the value of the id property for an entry
;; (org-entry-get (point) "issue_id")
;; (setq projects (elmine/get-projects :query_id 298))

;; (setq issues (elmine/get-issues :query_id 123))

;; (setq test (elmine/get-issue 12595 :include "journals"))
;; (elmine/get test :journals)

;; (setq j (car (elmine/get test :journals)))

;;(setq projects (elmine/get-projects :query_id 298))
