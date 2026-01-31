;;; folgezettel-org-roam.el --- Automatic folgezettel backlink generation for org-roam -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Blaine Mooers
;; Version: 2.3.0
;; Package-Requires: ((emacs "27.1") (org-roam "2.0"))
;; Keywords: org-mode, org-roam, zettelkasten, folgezettel
;; URL: https://github.com/MooersLab/folgezettel-org-roam

;;; Commentary:

;; This package provides automatic backlink generation for org-roam notes
;; that use folgezettel indexing (e.g., 1.2a3c5d7a, 1.13aa, 1.2a15).
;;
;; When you create a new note with a folgezettel in its title, this package
;; will automatically:
;; 1. Parse the folgezettel to identify the parent note's address
;; 2. Search the org-roam database for the parent note
;; 3. Insert a backlink to the parent note under "** Parent Note" in the child
;; 4. Insert a forward link to the child note under "** Child Notes" in the parent
;;
;; When using `folgezettel-org-roam-insert-next-child' to create a child note:
;; - The parent node is stored before capture begins
;; - After the capture is finalized, bidirectional links are automatically created
;; - The child note gets a link to the parent under "** Parent Note"
;; - The parent note gets a link to the new child under "** Child Notes"
;;
;; Additionally, when you manually insert a link to another org-roam note,
;; this package will automatically create a reciprocal link in the target
;; note (bidirectional cross-linking). This behavior can be controlled via
;; the `folgezettel-org-roam-auto-crosslink' customization variable.
;;
;; Example folgezettel hierarchy:
;;   1           - First note in chain
;;   1.2         - Second subtopic of note 1
;;   1.13        - Thirteenth subtopic of note 1
;;   1.13a       - First letter-indexed child of 1.13
;;   1.13b       - Second letter-indexed child of 1.13
;;   1.13aa      - 27th letter-indexed child of 1.13 (after z comes aa)
;;   1.13a2      - Second numeric child of 1.13a
;;   1.13a2b     - Second letter-indexed child of 1.13a2
;;
;; Parent-child relationships:
;;   1.13 is parent of: 1.13a, 1.13b, ..., 1.13z, 1.13aa, 1.13ab, ...
;;   1.13a is parent of: 1.13a1, 1.13a2, 1.13a3, ...
;;   1.13a2 is parent of: 1.13a2a, 1.13a2b, ..., 1.13a2z, 1.13a2aa, ...
;;
;; Installation:
;; 1. Place this file in your load-path
;; 2. Add to your init.el:
;;    (require 'folgezettel-org-roam)
;;    (folgezettel-org-roam-mode 1)

;;; Code:

(require 'org-roam)
(require 'org-roam-db)

(defgroup folgezettel-org-roam nil
  "Automatic folgezettel backlink generation for org-roam."
  :group 'org-roam
  :prefix "folgezettel-org-roam-")

(defcustom folgezettel-org-roam-parent-link-description "Parent note"
  "Description text for automatically generated parent links."
  :type 'string
  :group 'folgezettel-org-roam)

(defcustom folgezettel-org-roam-child-link-description nil
  "Description text for automatically generated child links.
If nil, use the child note's title."
  :type '(choice (const :tag "Use child title" nil)
                 (string :tag "Custom description"))
  :group 'folgezettel-org-roam)

(defcustom folgezettel-org-roam-backlink-heading "Parent Note"
  "Heading under which to insert parent backlinks in child notes.
If nil, insert at the beginning of the buffer after properties."
  :type '(choice (const :tag "No heading" nil)
                 (string :tag "Heading name"))
  :group 'folgezettel-org-roam)

(defcustom folgezettel-org-roam-forward-link-heading "Child Notes"
  "Heading under which to insert child forward links in parent notes.
If nil, insert at the end of the buffer."
  :type '(choice (const :tag "At end of buffer" nil)
                 (string :tag "Heading name"))
  :group 'folgezettel-org-roam)

(defcustom folgezettel-org-roam-regex
  "\\b\\([0-9]+\\(?:[.][0-9]+\\)*\\(?:[a-z]+\\(?:[0-9]+\\)?\\)*\\)\\(?:[^a-z0-9]\\|$\\)"
  "Regular expression to match folgezettel patterns.
Matches patterns like: 1, 1.2, 1.13, 1.2a, 1.2aa, 1.2a15, 1.2a3c5d7a
The pattern ensures complete matches by requiring non-alphanumeric or end of string after the pattern."
  :type 'regexp
  :group 'folgezettel-org-roam)

(defcustom folgezettel-org-roam-auto-crosslink t
  "Automatically create bidirectional links when inserting cross-links.
When non-nil, inserting a link to another org-roam note will automatically
insert a reciprocal link in the target note."
  :type 'boolean
  :group 'folgezettel-org-roam)

(defcustom folgezettel-org-roam-crosslink-heading "Cross References"
  "Heading under which to insert reciprocal cross-links.
If nil, insert at the end of the buffer."
  :type '(choice (const :tag "At end of buffer" nil)
                 (string :tag "Heading name"))
  :group 'folgezettel-org-roam)

(defcustom folgezettel-org-roam-sync-db-before-queries t
  "Sync org-roam database before queries in interactive functions.
When non-nil, interactive commands will sync the org-roam database before
querying for parent nodes or checking for duplicates. This ensures the most
current data is available but may add a slight delay.
Recommended: t (enabled) for best reliability."
  :type 'boolean
  :group 'folgezettel-org-roam)

(defvar folgezettel-org-roam--pending-parent-node nil
  "Stores the parent node when creating a child note via `folgezettel-org-roam-insert-next-child'.
This ensures the parent information is available when the capture hook fires.")

(defvar folgezettel-org-roam--pending-child-title nil
  "Stores the full title of the child note being created.
Used to verify the correct note is being processed in the capture hook.")

(defun folgezettel-org-roam--maybe-sync-db ()
  "Sync org-roam database if `folgezettel-org-roam-sync-db-before-queries' is non-nil."
  (when folgezettel-org-roam-sync-db-before-queries
    (org-roam-db-sync)))



(defun folgezettel-org-roam--parse-address (folgezettel)
  "Parse FOLGEZETTEL string and return the parent's address.

The pattern alternates between numbers and letters:
- Numbers after dots indicate subtopics
- Letters indicate alphabetic children
- Numbers after letters indicate numeric children

Examples:
  1.2a3c5   -> 1.2a3c   (remove last number)
  1.13aa    -> 1.13     (remove all trailing letters)
  1.13a     -> 1.13     (remove all trailing letters)
  1.2a      -> 1.2      (remove all trailing letters)
  1.13      -> 1        (remove dot and number)
  1         -> nil      (no parent)"
  (when (and folgezettel (string-match folgezettel-org-roam-regex folgezettel))
    (let ((addr folgezettel))
      ;; Remove the last component (number or letter sequence)
      (cond
       ;; Ends with numbers after letters (e.g., "1.2a15" -> "1.2a")
       ((string-match "\\(.*[a-z]+\\)[0-9]+$" addr)
        (match-string 1 addr))
       ;; Ends with letters (e.g., "1.13aa" -> "1.13", "1.13a" -> "1.13")
       ;; Remove ALL trailing letters to get parent
       ((string-match "\\(.*?\\)[a-z]+$" addr)
        (match-string 1 addr))
       ;; Ends with numbers after a dot (e.g., "1.13" -> "1")
       ((string-match "\\(.*\\)[.][0-9]+$" addr)
        (match-string 1 addr))
       ;; Single number or number sequence (e.g., "1" -> nil, no parent)
       ((string-match "^[0-9]+$" addr)
        nil)
       (t nil)))))

(defun folgezettel-org-roam--extract-from-title (title)
  "Extract folgezettel pattern from TITLE string.
Returns the folgezettel if found, nil otherwise."
  (when (and title (string-match folgezettel-org-roam-regex title))
    (match-string 1 title)))

(defun folgezettel-org-roam--find-parent-node (parent-address)
  "Find org-roam node with PARENT-ADDRESS in its title.
Returns the node object if found, nil otherwise."
  (when parent-address
    (let ((nodes (org-roam-node-list)))
      (catch 'found
        (dolist (node nodes)
          (let* ((title (org-roam-node-title node))
                 (fz (folgezettel-org-roam--extract-from-title title)))
            (when (and fz (string= fz parent-address))
              (throw 'found node))))
        nil))))

(defun folgezettel-org-roam-diagnose-address (address)
  "Diagnose issues with finding a folgezettel ADDRESS.
Shows what nodes exist and what folgezettel patterns are extracted from them."
  (interactive "sEnter folgezettel address to diagnose: ")
  (let ((nodes (org-roam-node-list))
        (found-nodes '()))
    (message "Diagnosing address: %s" address)
    (message "Searching through %d nodes..." (length nodes))
    (dolist (node nodes)
      (let* ((title (org-roam-node-title node))
             (fz (folgezettel-org-roam--extract-from-title title)))
        (when fz
          (push (cons fz title) found-nodes)
          (when (string= fz address)
            (message "FOUND MATCH: '%s' extracted from title '%s'" fz title)))))
    (message "---")
    (message "All folgezettel patterns found in vault:")
    (dolist (pair (reverse found-nodes))
      (message "  %s -> %s" (car pair) (cdr pair)))
    (message "---")
    (unless (assoc address found-nodes)
      (message "No node found with folgezettel '%s'" address))))

(defun folgezettel-org-roam--index-exists-p (address)
  "Check if a folgezettel ADDRESS already exists in the vault.
Returns the node if found, nil otherwise."
  (when address
    (let ((nodes (org-roam-node-list)))
      (catch 'found
        (dolist (node nodes)
          (let* ((title (org-roam-node-title node))
                 (fz (folgezettel-org-roam--extract-from-title title)))
            (when (and fz (string= fz address))
              (throw 'found node))))
        nil))))

(defun folgezettel-org-roam-check-duplicate-index (address)
  "Check if ADDRESS is already used and warn the user if so.
Returns t if the address is available, nil if it's already used.
Shows a warning popup if the address is already in use."
  (interactive "sEnter folgezettel address to check: ")
  ;; Sync database when called interactively
  (when (called-interactively-p 'any)
    (folgezettel-org-roam--maybe-sync-db))
  (let ((existing-node (folgezettel-org-roam--index-exists-p address)))
    (if existing-node
        (progn
          (display-warning 
           'folgezettel-org-roam
           (format "Duplicate index detected!\n\nThe folgezettel address '%s' is already used by:\n\n  \"%s\"\n\nPlease choose a different address."
                   address
                   (org-roam-node-title existing-node))
           :warning)
          nil)
      (when (called-interactively-p 'any)
        (message "Address '%s' is available." address))
      t)))

(defun folgezettel-org-roam--insert-backlink (parent-node)
  "Insert a backlink to PARENT-NODE in the current buffer."
  (let* ((parent-id (org-roam-node-id parent-node))
         (parent-title (org-roam-node-title parent-node))
         (description (or folgezettel-org-roam-parent-link-description parent-title)))
    (save-excursion
      (goto-char (point-min))
      ;; Skip past file-level properties
      (while (and (not (eobp))
                  (or (looking-at "^#\\+")
                      (looking-at "^:[A-Z]+:")
                      (looking-at "^$")))
        (forward-line 1))
      
      ;; If heading is specified, find or create it
      (if folgezettel-org-roam-backlink-heading
          (progn
            (unless (re-search-forward 
                     (concat "^\\*+ " (regexp-quote folgezettel-org-roam-backlink-heading))
                     nil t)
              ;; Heading does not exist, create it
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (insert "\n* " folgezettel-org-roam-backlink-heading "\n"))
            (end-of-line)
            (insert "\n"))
        ;; No heading, insert right after properties
        (unless (bolp) (insert "\n")))
      
      ;; Insert the backlink - use description only if non-empty
      (if (and description (not (string-empty-p description)))
          (insert (format "[[id:%s][%s]]\n" parent-id description))
        (insert (format "[[id:%s]]\n" parent-id))))))

(defun folgezettel-org-roam--insert-forward-link (child-node parent-file)
  "Insert a forward link to CHILD-NODE in PARENT-FILE."
  (let* ((child-id (org-roam-node-id child-node))
         (child-title (org-roam-node-title child-node))
         (link-desc (or folgezettel-org-roam-child-link-description child-title)))
    (with-current-buffer (find-file-noselect parent-file)
      (save-excursion
        (goto-char (point-max))
        
        ;; If heading is specified, find or create it
        (if folgezettel-org-roam-forward-link-heading
            (progn
              (goto-char (point-min))
              (unless (re-search-forward 
                       (concat "^\\*+ " (regexp-quote folgezettel-org-roam-forward-link-heading))
                       nil t)
                ;; Heading does not exist, create it at end
                (goto-char (point-max))
                (unless (bolp) (insert "\n"))
                (insert "\n* " folgezettel-org-roam-forward-link-heading "\n"))
              (end-of-line)
              (insert "\n"))
          ;; No heading, insert at end of buffer
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert "\n"))
        
        ;; Insert the forward link - use description only if non-empty
        (if (and link-desc (not (string-empty-p link-desc)))
            (insert (format "[[id:%s][%s]]\n" child-id link-desc))
          (insert (format "[[id:%s]]\n" child-id))))
      (save-buffer)
      ;; Update org-roam database for this file
      (org-roam-db-update-file parent-file))))

(defun folgezettel-org-roam--insert-crosslink (target-node source-node target-file)
  "Insert a reciprocal cross-link to SOURCE-NODE in TARGET-FILE.
TARGET-NODE is the node receiving the reciprocal link.
SOURCE-NODE is the node that initiated the link."
  (let* ((source-id (org-roam-node-id source-node))
         (source-title (org-roam-node-title source-node)))
    (with-current-buffer (find-file-noselect target-file)
      (save-excursion
        ;; If heading is specified, find or create it
        (if folgezettel-org-roam-crosslink-heading
            (progn
              (goto-char (point-min))
              (unless (re-search-forward 
                       (concat "^\\*+ " (regexp-quote folgezettel-org-roam-crosslink-heading))
                       nil t)
                ;; Heading does not exist, create it at end
                (goto-char (point-max))
                (unless (bolp) (insert "\n"))
                (insert "\n* " folgezettel-org-roam-crosslink-heading "\n"))
              (end-of-line)
              (insert "\n"))
          ;; No heading, insert at end of buffer
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert "\n"))
        
        ;; Check if reciprocal link already exists
        (let ((link-pattern (concat "\\[\\[id:" source-id "\\]")))
          (unless (save-excursion
                    (goto-char (point-min))
                    (re-search-forward link-pattern nil t))
            ;; Insert the reciprocal link - use title only if non-empty
            (if (and source-title (not (string-empty-p source-title)))
                (insert (format "[[id:%s][%s]]\n" source-id source-title))
              (insert (format "[[id:%s]]\n" source-id))))))
      (save-buffer)
      ;; Update org-roam database for this file
      (org-roam-db-update-file target-file))))

(defun folgezettel-org-roam--process-new-node ()
  "Process newly created org-roam node for folgezettel backlinks.
This function is called after a new node is created.
Creates bidirectional links:
- A link to the parent under '** Parent Note' in the child
- A link to the child under '** Child Notes' in the parent"
  (when-let* ((current-node (org-roam-node-at-point))
              (title (org-roam-node-title current-node))
              (folgezettel (folgezettel-org-roam--extract-from-title title))
              (parent-addr (folgezettel-org-roam--parse-address folgezettel)))
    ;; Use stored parent node if available and title matches, otherwise find it
    (let ((parent-node 
           (if (and folgezettel-org-roam--pending-parent-node
                    folgezettel-org-roam--pending-child-title
                    (string= title folgezettel-org-roam--pending-child-title))
               folgezettel-org-roam--pending-parent-node
             (folgezettel-org-roam--find-parent-node parent-addr))))
      (when parent-node
        ;; Insert backlink in current (child) note under "** Parent Note"
        (folgezettel-org-roam--insert-backlink parent-node)
        (save-buffer)
        ;; Update org-roam database for current (child) file
        (when-let ((current-file (org-roam-node-file current-node)))
          (org-roam-db-update-file current-file))
        
        ;; Insert forward link in parent note under "** Child Notes"
        (when-let ((parent-file (org-roam-node-file parent-node)))
          (folgezettel-org-roam--insert-forward-link current-node parent-file))
        
        (message "Created bidirectional links between '%s' and parent '%s'" 
                 title (org-roam-node-title parent-node)))
      
      ;; Clear the pending variables
      (setq folgezettel-org-roam--pending-parent-node nil)
      (setq folgezettel-org-roam--pending-child-title nil))))

(defun folgezettel-org-roam-add-backlink-to-parent ()
  "Manually add a backlink to the parent note based on folgezettel in title.
Also adds a forward link in the parent note.
Useful for existing notes or when automatic insertion fails."
  (interactive)
  ;; Sync database to ensure we have current data
  (folgezettel-org-roam--maybe-sync-db)
  (let ((current-node (org-roam-node-at-point)))
    (if (not current-node)
        (message "No org-roam node found at point")
      (let ((title (org-roam-node-title current-node)))
        (if (not title)
            (message "Current node has no title")
          (let ((folgezettel (folgezettel-org-roam--extract-from-title title)))
            (if (not folgezettel)
                (message "No folgezettel pattern found in title: %s" title)
              (let ((parent-addr (folgezettel-org-roam--parse-address folgezettel)))
                (if (not parent-addr)
                    (message "Could not determine parent address for folgezettel: %s (this may be a root note with no parent)" folgezettel)
                  (let ((parent-node (folgezettel-org-roam--find-parent-node parent-addr)))
                    (if (not parent-node)
                        (message "Could not find parent note with address '%s'. Make sure a note exists with this folgezettel in its title." parent-addr)
                      ;; Insert backlink in current note
                      (folgezettel-org-roam--insert-backlink parent-node)
                      (save-buffer)
                      ;; Update org-roam database for current file
                      (when-let ((current-file (org-roam-node-file current-node)))
                        (org-roam-db-update-file current-file))
                      ;; Insert forward link in parent note (this will update parent's DB entry)
                      (when-let ((parent-file (org-roam-node-file parent-node)))
                        (folgezettel-org-roam--insert-forward-link current-node parent-file))
                      (message "Inserted bidirectional links with parent note: %s" 
                               (org-roam-node-title parent-node)))))))))))))

;;; Validation Functions

(defun folgezettel-org-roam--validate-no-multiple-periods (address)
  "Check that ADDRESS contains at most one period (after the root number).
Returns nil if valid, or an error message string if invalid."
  (when (and address (stringp address))
    (let ((period-count (length (seq-filter (lambda (c) (= c ?.)) address))))
      (when (> period-count 1)
        (format "Invalid address '%s': Only one period is allowed after the root number. Found %d periods."
                address period-count)))))

(defun folgezettel-org-roam--validate-no-invalid-characters (address)
  "Check that ADDRESS contains only valid characters (digits, lowercase letters, single period).
Returns nil if valid, or an error message string if invalid."
  (when (and address (stringp address))
    (let ((invalid-chars '())
          (case-fold-search nil))  ; Ensure case-sensitive matching
      ;; Check for specific problematic characters for filenames
      (when (string-match-p "/" address)
        (push "/" invalid-chars))
      (when (string-match-p "\\\\" address)
        (push "\\" invalid-chars))
      (when (string-match-p "," address)
        (push "," invalid-chars))
      (when (string-match-p "!" address)
        (push "!" invalid-chars))
      (when (string-match-p "<" address)
        (push "<" invalid-chars))
      (when (string-match-p ">" address)
        (push ">" invalid-chars))
      (when (string-match-p ";" address)
        (push ";" invalid-chars))
      (when (string-match-p "&" address)
        (push "&" invalid-chars))
      (when (string-match-p "\\$" address)
        (push "$" invalid-chars))
      (when (string-match-p "\\*" address)
        (push "*" invalid-chars))
      (when (string-match-p "\\?" address)
        (push "?" invalid-chars))
      (when (string-match-p "{" address)
        (push "{" invalid-chars))
      (when (string-match-p "}" address)
        (push "}" invalid-chars))
      (when (string-match-p "`" address)
        (push "`" invalid-chars))
      (when (string-match-p "'" address)
        (push "'" invalid-chars))
      (when (string-match-p "\"" address)
        (push "\"" invalid-chars))
      ;; Check for any other invalid characters (not digit, lowercase letter, or period)
      ;; Use case-sensitive replacement
      (let ((cleaned (replace-regexp-in-string "[0-9a-z.]" "" address)))
        (when (> (length cleaned) 0)
          (dolist (char (string-to-list cleaned))
            (unless (member (char-to-string char) invalid-chars)
              (push (char-to-string char) invalid-chars)))))
      (when invalid-chars
        (format "Invalid address '%s': Contains invalid character(s): %s. Only digits (0-9), lowercase letters (a-z), and a single period are allowed."
                address (string-join (reverse invalid-chars) ", "))))))

(defun folgezettel-org-roam--validate-alternation-pattern (address)
  "Check that ADDRESS follows the alternation pattern (numbers and letters must alternate).
After the initial number and optional .number, the pattern must alternate:
- Letter segments must follow number segments
- Number segments must follow letter segments
Note: Extended alphabet sequences like 'aa', 'ab', 'aaa' are valid as single letter segments.
Returns nil if valid, or an error message string if invalid."
  (when (and address (stringp address))
    ;; First, extract the part after the root (e.g., from "1.2abc3d" get "abc3d")
    (when (string-match "^[0-9]+\\(?:\\.[0-9]+\\)?\\(.*\\)$" address)
      (let ((suffix (match-string 1 address)))
        (when (> (length suffix) 0)
          ;; Parse the suffix into segments of contiguous letters or numbers
          (let ((segments '())
                (pos 0)
                (len (length suffix)))
            (while (< pos len)
              (cond
               ;; Letter sequence (including extended like aa, ab, aaa)
               ((string-match "\\`[a-z]+" (substring suffix pos))
                (let ((match (match-string 0 (substring suffix pos))))
                  (push (cons 'letters match) segments)
                  (setq pos (+ pos (length match)))))
               ;; Number sequence
               ((string-match "\\`[0-9]+" (substring suffix pos))
                (let ((match (match-string 0 (substring suffix pos))))
                  (push (cons 'numbers match) segments)
                  (setq pos (+ pos (length match)))))
               ;; Unexpected character (shouldn't happen if other validations pass)
               (t (setq pos len))))
            
            ;; Check alternation: letter segments and number segments should alternate
            ;; Valid: letters -> numbers -> letters -> numbers ...
            ;; Invalid: letters -> letters or numbers -> numbers
            (setq segments (reverse segments))
            (let ((prev-type nil)
                  (error-msg nil))
              (dolist (seg segments)
                (let ((seg-type (car seg))
                      (seg-value (cdr seg)))
                  (when (and prev-type (eq prev-type seg-type))
                    (setq error-msg
                          (if (eq seg-type 'numbers)
                              (format "Invalid address '%s': Number segment '%s' cannot directly follow another number segment. Use a letter between numeric segments (e.g., '1.2a3' not '1.23' for a child of '1.2')."
                                      address seg-value)
                            ;; This case shouldn't happen with proper parsing since
                            ;; consecutive letters are grouped together
                            (format "Invalid address '%s': Consecutive letter segments detected. This is an internal error."
                                    address))))
                  (setq prev-type seg-type)))
              error-msg)))))))

(defun folgezettel-org-roam--validate-child-for-parent (parent-address child-suffix)
  "Validate that CHILD-SUFFIX is appropriate for PARENT-ADDRESS.
Returns nil if valid, or an error message string if invalid.

Rules:
- If parent ends with a number, child must start with a letter OR be .number
- If parent ends with a letter, child must start with a number"
  (when (and parent-address child-suffix
             (stringp parent-address) (stringp child-suffix)
             (> (length parent-address) 0) (> (length child-suffix) 0))
    (let ((parent-last-char (aref parent-address (1- (length parent-address))))
          (child-first-char (aref child-suffix 0)))
      (cond
       ;; Parent ends with a number
       ((and (>= parent-last-char ?0) (<= parent-last-char ?9))
        (cond
         ;; Child starts with a letter - valid
         ((and (>= child-first-char ?a) (<= child-first-char ?z))
          nil)
         ;; Child starts with a dot (numeric child) - valid
         ((= child-first-char ?.)
          nil)
         ;; Child starts with a number (without dot) - invalid
         ((and (>= child-first-char ?0) (<= child-first-char ?9))
          (format "Invalid child suffix '%s' for parent '%s': Parent ends with a number, so child must start with a letter (e.g., '%sa') or use a dot for numeric children (e.g., '%s.1')."
                  child-suffix parent-address parent-address parent-address))
         (t nil)))
       ;; Parent ends with a letter
       ((and (>= parent-last-char ?a) (<= parent-last-char ?z))
        (cond
         ;; Child starts with a number - valid
         ((and (>= child-first-char ?0) (<= child-first-char ?9))
          nil)
         ;; Child starts with a letter - invalid (can't add letter to letter-ending parent)
         ((and (>= child-first-char ?a) (<= child-first-char ?z))
          (format "Invalid child suffix '%s' for parent '%s': Parent ends with a letter, so child must start with a number (e.g., '%s1'). You cannot add a letter child to a letter-ending parent."
                  child-suffix parent-address parent-address))
         (t nil)))
       (t nil)))))

(defun folgezettel-org-roam-validate-address (address)
  "Validate that ADDRESS follows proper folgezettel format.
Returns t if valid, nil otherwise.
Use `folgezettel-org-roam-validate-address-full' for detailed error messages."
  (and address
       (stringp address)
       (not (folgezettel-org-roam--validate-no-invalid-characters address))
       (not (folgezettel-org-roam--validate-no-multiple-periods address))
       (not (folgezettel-org-roam--validate-alternation-pattern address))
       (string-match-p "^[0-9]+\\(?:\\.[0-9]+\\)?\\(?:[a-z]+[0-9]*\\)*$" address)))

(defun folgezettel-org-roam-validate-address-full (address)
  "Validate ADDRESS and return detailed error information.
Returns nil if valid, or a list of error message strings if invalid."
  (let ((errors '()))
    (unless (and address (stringp address))
      (push "Address must be a non-empty string." errors))
    (when (and address (stringp address))
      ;; Check for invalid characters
      (when-let ((err (folgezettel-org-roam--validate-no-invalid-characters address)))
        (push err errors))
      ;; Check for multiple periods
      (when-let ((err (folgezettel-org-roam--validate-no-multiple-periods address)))
        (push err errors))
      ;; Check alternation pattern
      (when-let ((err (folgezettel-org-roam--validate-alternation-pattern address)))
        (push err errors))
      ;; Check basic structure (must start with a number)
      (unless (string-match-p "^[0-9]" address)
        (push (format "Invalid address '%s': Must start with a number." address) errors)))
    (reverse errors)))

(defun folgezettel-org-roam-validate-new-child (parent-address child-address)
  "Validate that CHILD-ADDRESS is a valid child of PARENT-ADDRESS.
Returns nil if valid, or a list of error message strings if invalid."
  (let ((errors '()))
    ;; First validate both addresses individually
    (let ((parent-errors (folgezettel-org-roam-validate-address-full parent-address))
          (child-errors (folgezettel-org-roam-validate-address-full child-address)))
      (when parent-errors
        (push (format "Parent address errors: %s" (string-join parent-errors "; ")) errors))
      (when child-errors
        (push (format "Child address errors: %s" (string-join child-errors "; ")) errors)))
    
    ;; Then validate parent-child relationship
    (when (and (null errors)
               parent-address child-address
               (stringp parent-address) (stringp child-address))
      ;; Child must start with parent
      (unless (string-prefix-p parent-address child-address)
        (push (format "Child '%s' must start with parent address '%s'."
                      child-address parent-address) errors))
      ;; Validate the suffix
      (when (string-prefix-p parent-address child-address)
        (let ((suffix (substring child-address (length parent-address))))
          (when (= (length suffix) 0)
            (push "Child address cannot be identical to parent address." errors))
          (when (> (length suffix) 0)
            (when-let ((err (folgezettel-org-roam--validate-child-for-parent 
                            parent-address suffix)))
              (push err errors))))))
    (reverse errors)))

(defun folgezettel-org-roam-report-validation-errors (address)
  "Interactively validate ADDRESS and display any errors.
Returns t if valid, nil if invalid."
  (interactive "sEnter folgezettel address to validate: ")
  (let ((errors (folgezettel-org-roam-validate-address-full address)))
    (if errors
        (progn
          (message "Validation FAILED for '%s':\n%s" 
                   address 
                   (string-join errors "\n"))
          nil)
      (message "Validation PASSED: '%s' is a valid folgezettel address." address)
      t)))

(defun folgezettel-org-roam--next-letter-sequence (current)
  "Return the next letter sequence after CURRENT.
Examples: a -> b, z -> aa, az -> ba, zz -> aaa"
  (let* ((chars (string-to-list current))
         (result (reverse chars))
         (carry t))
    (setq result
          (mapcar (lambda (c)
                    (if carry
                        (if (= c ?z)
                            (progn ?a)
                          (progn 
                            (setq carry nil)
                            (1+ c)))
                      c))
                  result))
    (when carry
      (setq result (cons ?a result)))
    (concat (reverse result))))

(defun folgezettel-org-roam-suggest-next-child (parent-address)
  "Suggest the next child address for PARENT-ADDRESS.
Returns a list of valid suggestions based on the parent's type.

Addressing rules:
- If parent is a root number (e.g., '7'): can ONLY add dot-number child (7.1)
- If parent has a dot already (e.g., '7.1'): can ONLY add letter child (7.1a)
- If parent ends with a letter (e.g., '7.1a'): can ONLY add number child (7.1a1)

Returns nil with an error message if parent-address is invalid."
  (let ((errors (folgezettel-org-roam-validate-address-full parent-address)))
    (if errors
        (progn
          (message "Cannot suggest children: %s" (string-join errors "; "))
          nil)
      (let* ((nodes (org-roam-node-list))
             (children (seq-filter
                        (lambda (node)
                          (when-let ((title (org-roam-node-title node))
                                     (fz (folgezettel-org-roam--extract-from-title title)))
                            (and (string-prefix-p parent-address fz)
                                 (not (string= parent-address fz))
                                 (string= parent-address 
                                         (folgezettel-org-roam--parse-address fz)))))
                        nodes))
             (parent-ends-with-letter 
              (and (> (length parent-address) 0)
                   (let ((last-char (aref parent-address (1- (length parent-address)))))
                     (and (>= last-char ?a) (<= last-char ?z)))))
             (parent-is-root (string-match-p "^[0-9]+$" parent-address))
             (parent-has-dot (string-match-p "\\." parent-address))
             (max-num-with-dot 0)
             (max-num-after-letter 0)
             (max-letter nil))
        
        ;; Analyze existing children
        (dolist (child children)
          (when-let* ((title (org-roam-node-title child))
                      (fz (folgezettel-org-roam--extract-from-title title)))
            (cond
             ;; Numeric child with dot (e.g., "1.2" from parent "1")
             ((string-match (concat "^" (regexp-quote parent-address) 
                                    "[.]\\([0-9]+\\)") 
                            fz)
              (let ((num (string-to-number (match-string 1 fz))))
                (setq max-num-with-dot (max max-num-with-dot num))))
             
             ;; Numeric child after letter parent (e.g., "1a2" from parent "1a")
             ((and parent-ends-with-letter
                   (string-match (concat "^" (regexp-quote parent-address) 
                                        "\\([0-9]+\\)") 
                                fz))
              (let ((num (string-to-number (match-string 1 fz))))
                (setq max-num-after-letter (max max-num-after-letter num))))
             
             ;; Letter child (e.g., "1.2a" from "1.2")
             ((and (not parent-ends-with-letter)
                   parent-has-dot
                   (string-match (concat "^" (regexp-quote parent-address) 
                                        "\\([a-z]+\\)") 
                                fz))
              (let ((letters (match-string 1 fz)))
                (setq max-letter 
                      (if (or (not max-letter)
                              (string< max-letter letters))
                          letters
                        max-letter)))))))
        
        ;; Generate suggestions based on parent type
        (cond
         ;; Parent ends with letter: can ONLY add number child (e.g., 1.2a -> 1.2a1)
         (parent-ends-with-letter
          (let ((numeric-suggestion
                 (concat parent-address (number-to-string (1+ max-num-after-letter)))))
            (list numeric-suggestion)))
         
         ;; Parent is root number (no dot): can ONLY add dot-number child
         ;; e.g., 7 -> 7.1 (NOT 7a)
         (parent-is-root
          (let ((numeric-suggestion
                 (concat parent-address "." (number-to-string (1+ max-num-with-dot)))))
            (list numeric-suggestion)))
         
         ;; Parent already has a dot (e.g., 7.1): can ONLY add letter child
         ;; e.g., 7.1 -> 7.1a (NOT 7.1.1 which would have two dots)
         (parent-has-dot
          (let ((alphabetic-suggestion
                 (concat parent-address 
                         (if max-letter
                             (folgezettel-org-roam--next-letter-sequence max-letter)
                           "a"))))
            (list alphabetic-suggestion)))
         
         ;; Fallback (shouldn't reach here with valid addresses)
         (t nil))))))


(defun folgezettel-org-roam-insert-next-child ()
  "Interactively create a new child note with suggested folgezettel.
Prompts for numeric or alphabetic suffix, validates the choice,
checks for duplicate indexes, prompts for a title, and creates the note.
Automatically creates bidirectional links between the new child and its parent:
- A link to the parent under '** Parent Note' in the child
- A link to the child under '** Child Notes' in the parent"
  (interactive)
  ;; Sync database to ensure we have current data
  (folgezettel-org-roam--maybe-sync-db)
  (if-let* ((current-node (org-roam-node-at-point))
            (title (org-roam-node-title current-node))
            (current-fz (folgezettel-org-roam--extract-from-title title)))
      (let ((suggestions (folgezettel-org-roam-suggest-next-child current-fz)))
        (if (null suggestions)
            (message "Could not generate suggestions for '%s'. Check if it's a valid address." current-fz)
          (let* ((choice (completing-read 
                          (format "Choose next child address (parent: %s): " current-fz)
                          suggestions
                          nil nil nil nil (car suggestions)))
                 (errors (folgezettel-org-roam-validate-new-child current-fz choice)))
            (cond
             ;; Validation errors
             (errors
              (message "Invalid child address '%s':\n%s" choice (string-join errors "\n"))
              (when (y-or-n-p "Would you like to try again with a valid address? ")
                (folgezettel-org-roam-insert-next-child)))
             ;; Duplicate index
             ((not (folgezettel-org-roam-check-duplicate-index choice))
              (when (y-or-n-p "Would you like to try again with a different address? ")
                (folgezettel-org-roam-insert-next-child)))
             ;; Valid and available - prompt for title and create the note
             (t
              (let* ((note-title (read-string (format "Enter title for %s: " choice)))
                     (full-title (if (string-empty-p note-title)
                                     choice
                                   (concat choice " " note-title))))
                ;; Store parent node information for the capture hook
                (setq folgezettel-org-roam--pending-parent-node current-node)
                (setq folgezettel-org-roam--pending-child-title full-title)
                (org-roam-capture- :node (org-roam-node-create :title full-title))))))))
    (message "Current note does not have a valid folgezettel address in title: %s" 
             (if (and (org-roam-node-at-point) 
                      (org-roam-node-title (org-roam-node-at-point)))
                 (org-roam-node-title (org-roam-node-at-point))
               "(no node at point)"))))

(defun folgezettel-org-roam--handle-crosslink-insertion (link &optional description)
  "Handle bidirectional link creation after inserting LINK with DESCRIPTION.
This function is called via advice after `org-insert-link'."
  (when (and folgezettel-org-roam-auto-crosslink
             (org-roam-node-at-point)
             link
             (string-match "^id:\\(.+\\)$" link))
    (let* ((target-id (match-string 1 link))
           (target-node (org-roam-node-from-id target-id))
           (source-node (org-roam-node-at-point)))
      (when (and target-node source-node
                 (not (string= (org-roam-node-id source-node)
                              (org-roam-node-id target-node))))
        (let ((target-file (org-roam-node-file target-node)))
          (when target-file
            (folgezettel-org-roam--insert-crosslink target-node source-node target-file)
            (message "Created reciprocal link in: %s" 
                     (org-roam-node-title target-node))))))))

(defun folgezettel-org-roam--org-insert-link-advice (orig-fun &rest args)
  "Advice function to wrap `org-insert-link' for bidirectional linking.
ORIG-FUN is the original `org-insert-link' function.
ARGS are the arguments passed to `org-insert-link'."
  (let ((result (apply orig-fun args)))
    ;; After inserting link, check if it is an org-roam ID link
    (save-excursion
      (when (and (org-in-regexp org-link-bracket-re 1)
                 (match-string 1))
        (let ((link-data (match-string 1)))
          (when (string-match "^id:\\(.+\\)$" link-data)
            (folgezettel-org-roam--handle-crosslink-insertion 
             link-data 
             (match-string 2))))))
    result))

;;;###autoload
(define-minor-mode folgezettel-org-roam-mode
  "Minor mode for automatic folgezettel backlink generation in org-roam."
  :global t
  :group 'folgezettel-org-roam
  :lighter " FZ"
  (if folgezettel-org-roam-mode
      (progn
        (add-hook 'org-roam-capture-new-node-hook 
                  #'folgezettel-org-roam--process-new-node)
        (advice-add 'org-insert-link :around 
                    #'folgezettel-org-roam--org-insert-link-advice))
    (remove-hook 'org-roam-capture-new-node-hook 
                 #'folgezettel-org-roam--process-new-node)
    (advice-remove 'org-insert-link 
                   #'folgezettel-org-roam--org-insert-link-advice)))

(provide 'folgezettel-org-roam)

;;; folgezettel-org-roam.el ends here
