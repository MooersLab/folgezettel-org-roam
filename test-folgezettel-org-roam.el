;;; test-folgezettel-org-roam.el --- Tests for folgezettel-org-roam -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Blaine Mooers

;;; Commentary:

;; Comprehensive test suite for folgezettel-org-roam.el using ERT.
;; Run all tests with: M-x ert RET t RET
;; Or from command line: emacs -batch -l ert -l folgezettel-org-roam.el -l test-folgezettel-org-roam.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load the package being tested
(require 'folgezettel-org-roam)

;;; ============================================================================
;;; Test Utilities and Mock Infrastructure
;;; ============================================================================

(defvar test-folgezettel-org-roam--temp-dir nil
  "Temporary directory for test files.")

(defvar test-folgezettel-org-roam--mock-nodes nil
  "List of mock nodes for testing.")

(defun test-folgezettel-org-roam--create-mock-node (id title file)
  "Create a mock org-roam node with ID, TITLE, and FILE."
  (org-roam-node-create :id id :title title :file file))

(defmacro test-folgezettel-org-roam--with-temp-dir (&rest body)
  "Execute BODY with a temporary directory set up and cleaned up."
  `(let ((test-folgezettel-org-roam--temp-dir (make-temp-file "folgezettel-test-" t)))
     (unwind-protect
         (progn ,@body)
       (when (and test-folgezettel-org-roam--temp-dir
                  (file-exists-p test-folgezettel-org-roam--temp-dir))
         (delete-directory test-folgezettel-org-roam--temp-dir t)))))

(defmacro test-folgezettel-org-roam--with-mock-nodes (nodes &rest body)
  "Execute BODY with NODES as the mock org-roam-node-list."
  `(cl-letf (((symbol-function 'org-roam-node-list) (lambda () ,nodes)))
     ,@body))

(defun test-folgezettel-org-roam--create-temp-org-file (filename content)
  "Create a temporary org file with FILENAME and CONTENT."
  (let ((filepath (expand-file-name filename test-folgezettel-org-roam--temp-dir)))
    (with-temp-file filepath
      (insert content))
    filepath))

;;; ============================================================================
;;; Unit Tests: folgezettel-org-roam--parse-address
;;; ============================================================================

(ert-deftest test-parse-address-single-number ()
  "Test parsing root addresses (single numbers have no parent)."
  (should (equal nil (folgezettel-org-roam--parse-address "1")))
  (should (equal nil (folgezettel-org-roam--parse-address "7")))
  (should (equal nil (folgezettel-org-roam--parse-address "42")))
  (should (equal nil (folgezettel-org-roam--parse-address "123"))))

(ert-deftest test-parse-address-dot-number ()
  "Test parsing dot-number addresses."
  (should (equal "1" (folgezettel-org-roam--parse-address "1.2")))
  (should (equal "1" (folgezettel-org-roam--parse-address "1.13")))
  (should (equal "7" (folgezettel-org-roam--parse-address "7.1")))
  (should (equal "42" (folgezettel-org-roam--parse-address "42.99"))))

(ert-deftest test-parse-address-single-letter ()
  "Test parsing addresses ending with single letter."
  (should (equal "1.2" (folgezettel-org-roam--parse-address "1.2a")))
  (should (equal "1.13" (folgezettel-org-roam--parse-address "1.13b")))
  (should (equal "7.1" (folgezettel-org-roam--parse-address "7.1z"))))

(ert-deftest test-parse-address-extended-letters ()
  "Test parsing addresses ending with extended letter sequences (aa, ab, etc.)."
  (should (equal "1.13" (folgezettel-org-roam--parse-address "1.13aa")))
  (should (equal "1.13" (folgezettel-org-roam--parse-address "1.13az")))
  (should (equal "1.13" (folgezettel-org-roam--parse-address "1.13ba")))
  (should (equal "1.2" (folgezettel-org-roam--parse-address "1.2aaa"))))

(ert-deftest test-parse-address-number-after-letter ()
  "Test parsing addresses ending with number after letter."
  (should (equal "1.2a" (folgezettel-org-roam--parse-address "1.2a1")))
  (should (equal "1.2a" (folgezettel-org-roam--parse-address "1.2a15")))
  (should (equal "1.13aa" (folgezettel-org-roam--parse-address "1.13aa5"))))

(ert-deftest test-parse-address-complex-chains ()
  "Test parsing complex folgezettel chains."
  (should (equal "1.2a3" (folgezettel-org-roam--parse-address "1.2a3b")))
  (should (equal "1.2a3b" (folgezettel-org-roam--parse-address "1.2a3b4")))
  (should (equal "1.2a3c5" (folgezettel-org-roam--parse-address "1.2a3c5d")))
  (should (equal "1.2a3c5d" (folgezettel-org-roam--parse-address "1.2a3c5d7")))
  (should (equal "1.2a3c5d7" (folgezettel-org-roam--parse-address "1.2a3c5d7a"))))

(ert-deftest test-parse-address-invalid-input ()
  "Test parsing invalid inputs."
  (should (equal nil (folgezettel-org-roam--parse-address nil)))
  (should (equal nil (folgezettel-org-roam--parse-address "")))
  (should (equal nil (folgezettel-org-roam--parse-address "abc"))))

;;; ============================================================================
;;; Unit Tests: folgezettel-org-roam--extract-from-title
;;; ============================================================================

(ert-deftest test-extract-from-title-basic ()
  "Test extracting folgezettel from basic titles."
  (should (equal "1" (folgezettel-org-roam--extract-from-title "1 Introduction")))
  (should (equal "1.2" (folgezettel-org-roam--extract-from-title "1.2 Methods")))
  (should (equal "1.2a" (folgezettel-org-roam--extract-from-title "1.2a Details"))))

(ert-deftest test-extract-from-title-complex ()
  "Test extracting complex folgezettel from titles."
  (should (equal "1.13" (folgezettel-org-roam--extract-from-title "1.13 Topic")))
  (should (equal "1.13aa" (folgezettel-org-roam--extract-from-title "1.13aa Extended")))
  (should (equal "1.2a3c5d7a" (folgezettel-org-roam--extract-from-title "1.2a3c5d7a Deep Chain"))))

(ert-deftest test-extract-from-title-address-only ()
  "Test extracting when title is just the address."
  (should (equal "7" (folgezettel-org-roam--extract-from-title "7")))
  (should (equal "7.1" (folgezettel-org-roam--extract-from-title "7.1")))
  (should (equal "7.1a" (folgezettel-org-roam--extract-from-title "7.1a"))))

(ert-deftest test-extract-from-title-no-match ()
  "Test extraction when no folgezettel is present."
  (should (equal nil (folgezettel-org-roam--extract-from-title "Introduction to Topic")))
  (should (equal nil (folgezettel-org-roam--extract-from-title "Notes on Methods")))
  (should (equal nil (folgezettel-org-roam--extract-from-title nil)))
  (should (equal nil (folgezettel-org-roam--extract-from-title ""))))

(ert-deftest test-extract-from-title-embedded-numbers ()
  "Test extraction does not match embedded numbers incorrectly."
  ;; Should match the leading folgezettel only
  (should (equal "1" (folgezettel-org-roam--extract-from-title "1 Chapter with 42 pages")))
  (should (equal "2.3" (folgezettel-org-roam--extract-from-title "2.3 Analysis of Year 2024"))))

;;; ============================================================================
;;; Unit Tests: folgezettel-org-roam--next-letter-sequence
;;; ============================================================================

(ert-deftest test-next-letter-sequence-basic ()
  "Test basic letter increment."
  (should (equal "b" (folgezettel-org-roam--next-letter-sequence "a")))
  (should (equal "c" (folgezettel-org-roam--next-letter-sequence "b")))
  (should (equal "z" (folgezettel-org-roam--next-letter-sequence "y"))))

(ert-deftest test-next-letter-sequence-wraparound ()
  "Test letter wraparound from z to aa."
  (should (equal "aa" (folgezettel-org-roam--next-letter-sequence "z"))))

(ert-deftest test-next-letter-sequence-double-letters ()
  "Test double letter sequences."
  (should (equal "ab" (folgezettel-org-roam--next-letter-sequence "aa")))
  (should (equal "az" (folgezettel-org-roam--next-letter-sequence "ay")))
  (should (equal "ba" (folgezettel-org-roam--next-letter-sequence "az")))
  (should (equal "bb" (folgezettel-org-roam--next-letter-sequence "ba"))))

(ert-deftest test-next-letter-sequence-zz-wraparound ()
  "Test wraparound from zz to aaa."
  (should (equal "aaa" (folgezettel-org-roam--next-letter-sequence "zz"))))

(ert-deftest test-next-letter-sequence-triple-letters ()
  "Test triple letter sequences."
  (should (equal "aab" (folgezettel-org-roam--next-letter-sequence "aaa")))
  (should (equal "baa" (folgezettel-org-roam--next-letter-sequence "azz"))))

;;; ============================================================================
;;; Unit Tests: Validation Functions
;;; ============================================================================

(ert-deftest test-validate-no-multiple-periods-valid ()
  "Test that single or no period is valid."
  (should (equal nil (folgezettel-org-roam--validate-no-multiple-periods "1")))
  (should (equal nil (folgezettel-org-roam--validate-no-multiple-periods "1.2")))
  (should (equal nil (folgezettel-org-roam--validate-no-multiple-periods "1.2a3b"))))

(ert-deftest test-validate-no-multiple-periods-invalid ()
  "Test that multiple periods are invalid."
  (should (stringp (folgezettel-org-roam--validate-no-multiple-periods "1.2.3")))
  (should (stringp (folgezettel-org-roam--validate-no-multiple-periods "1.2.3.4")))
  (should (string-match-p "Only one period" 
                          (folgezettel-org-roam--validate-no-multiple-periods "1.2.3"))))

(ert-deftest test-validate-no-invalid-characters-valid ()
  "Test valid characters pass validation."
  (should (equal nil (folgezettel-org-roam--validate-no-invalid-characters "1")))
  (should (equal nil (folgezettel-org-roam--validate-no-invalid-characters "1.2")))
  (should (equal nil (folgezettel-org-roam--validate-no-invalid-characters "1.2a3b4c")))
  (should (equal nil (folgezettel-org-roam--validate-no-invalid-characters "42.99zz"))))

(ert-deftest test-validate-no-invalid-characters-uppercase ()
  "Test uppercase letters are invalid."
  (should (stringp (folgezettel-org-roam--validate-no-invalid-characters "1.2A")))
  (should (stringp (folgezettel-org-roam--validate-no-invalid-characters "1.2aB"))))

(ert-deftest test-validate-no-invalid-characters-special ()
  "Test special characters are invalid."
  (should (stringp (folgezettel-org-roam--validate-no-invalid-characters "1.2a!")))
  (should (stringp (folgezettel-org-roam--validate-no-invalid-characters "1/2")))
  (should (stringp (folgezettel-org-roam--validate-no-invalid-characters "1,2")))
  (should (stringp (folgezettel-org-roam--validate-no-invalid-characters "1*2")))
  (should (stringp (folgezettel-org-roam--validate-no-invalid-characters "1?2"))))

(ert-deftest test-validate-alternation-pattern-valid ()
  "Test valid alternation patterns."
  (should (equal nil (folgezettel-org-roam--validate-alternation-pattern "1")))
  (should (equal nil (folgezettel-org-roam--validate-alternation-pattern "1.2")))
  (should (equal nil (folgezettel-org-roam--validate-alternation-pattern "1.2a")))
  (should (equal nil (folgezettel-org-roam--validate-alternation-pattern "1.2a3")))
  (should (equal nil (folgezettel-org-roam--validate-alternation-pattern "1.2a3b")))
  (should (equal nil (folgezettel-org-roam--validate-alternation-pattern "1.2aa")))
  (should (equal nil (folgezettel-org-roam--validate-alternation-pattern "1.2aaa3bbb"))))

(ert-deftest test-validate-child-for-parent-number-parent ()
  "Test child validation when parent ends with number."
  ;; Valid: letter child
  (should (equal nil (folgezettel-org-roam--validate-child-for-parent "1.2" "a")))
  (should (equal nil (folgezettel-org-roam--validate-child-for-parent "1.2" "aa")))
  ;; Valid: dot-number child
  (should (equal nil (folgezettel-org-roam--validate-child-for-parent "1" ".2")))
  ;; Invalid: bare number child (without dot)
  (should (stringp (folgezettel-org-roam--validate-child-for-parent "1.2" "3"))))

(ert-deftest test-validate-child-for-parent-letter-parent ()
  "Test child validation when parent ends with letter."
  ;; Valid: number child
  (should (equal nil (folgezettel-org-roam--validate-child-for-parent "1.2a" "1")))
  (should (equal nil (folgezettel-org-roam--validate-child-for-parent "1.2a" "15")))
  ;; Invalid: letter child
  (should (stringp (folgezettel-org-roam--validate-child-for-parent "1.2a" "b"))))

(ert-deftest test-validate-address-valid ()
  "Test complete address validation for valid addresses."
  (should (folgezettel-org-roam-validate-address "1"))
  (should (folgezettel-org-roam-validate-address "1.2"))
  (should (folgezettel-org-roam-validate-address "1.2a"))
  (should (folgezettel-org-roam-validate-address "1.2a3"))
  (should (folgezettel-org-roam-validate-address "1.13aa"))
  (should (folgezettel-org-roam-validate-address "42.99zz15")))

(ert-deftest test-validate-address-invalid ()
  "Test complete address validation for invalid addresses."
  (should (equal nil (folgezettel-org-roam-validate-address nil)))
  (should (equal nil (folgezettel-org-roam-validate-address "")))
  (should (equal nil (folgezettel-org-roam-validate-address "1.2.3")))
  (should (equal nil (folgezettel-org-roam-validate-address "1.2A")))
  (should (equal nil (folgezettel-org-roam-validate-address "abc"))))

(ert-deftest test-validate-address-full-returns-errors ()
  "Test that validate-address-full returns error list for invalid addresses."
  (should (equal nil (folgezettel-org-roam-validate-address-full "1.2a")))
  (should (listp (folgezettel-org-roam-validate-address-full "1.2.3")))
  (should (listp (folgezettel-org-roam-validate-address-full "1.2A")))
  (should (> (length (folgezettel-org-roam-validate-address-full "1.2.3!A")) 1)))

(ert-deftest test-validate-new-child-valid ()
  "Test validation of valid parent-child relationships."
  (should (equal nil (folgezettel-org-roam-validate-new-child "1" "1.1")))
  (should (equal nil (folgezettel-org-roam-validate-new-child "1.2" "1.2a")))
  (should (equal nil (folgezettel-org-roam-validate-new-child "1.2a" "1.2a1")))
  (should (equal nil (folgezettel-org-roam-validate-new-child "1.13" "1.13aa"))))

(ert-deftest test-validate-new-child-invalid ()
  "Test validation of invalid parent-child relationships."
  ;; Child does not start with parent
  (should (listp (folgezettel-org-roam-validate-new-child "1.2" "1.3a")))
  ;; Child identical to parent
  (should (listp (folgezettel-org-roam-validate-new-child "1.2" "1.2")))
  ;; Invalid child suffix
  (should (listp (folgezettel-org-roam-validate-new-child "1.2a" "1.2ab"))))

;;; ============================================================================
;;; Unit Tests: folgezettel-org-roam-suggest-next-child
;;; ============================================================================

(ert-deftest test-suggest-next-child-root-number ()
  "Test suggestions for root number parents."
  (test-folgezettel-org-roam--with-mock-nodes '()
    (let ((suggestions (folgezettel-org-roam-suggest-next-child "7")))
      (should (equal '("7.1") suggestions)))))

(ert-deftest test-suggest-next-child-root-with-existing ()
  "Test suggestions for root number with existing children."
  (let* ((child1 (test-folgezettel-org-roam--create-mock-node "id1" "7.1 First" "/tmp/7-1.org"))
         (child2 (test-folgezettel-org-roam--create-mock-node "id2" "7.2 Second" "/tmp/7-2.org"))
         (nodes (list child1 child2)))
    (test-folgezettel-org-roam--with-mock-nodes nodes
      (let ((suggestions (folgezettel-org-roam-suggest-next-child "7")))
        (should (equal '("7.3") suggestions))))))

(ert-deftest test-suggest-next-child-dot-number ()
  "Test suggestions for dot-number parents."
  (test-folgezettel-org-roam--with-mock-nodes '()
    (let ((suggestions (folgezettel-org-roam-suggest-next-child "7.1")))
      (should (equal '("7.1a") suggestions)))))

(ert-deftest test-suggest-next-child-dot-number-with-existing ()
  "Test suggestions for dot-number with existing letter children."
  (let* ((child1 (test-folgezettel-org-roam--create-mock-node "id1" "7.1a First" "/tmp/7-1a.org"))
         (child2 (test-folgezettel-org-roam--create-mock-node "id2" "7.1b Second" "/tmp/7-1b.org"))
         (nodes (list child1 child2)))
    (test-folgezettel-org-roam--with-mock-nodes nodes
      (let ((suggestions (folgezettel-org-roam-suggest-next-child "7.1")))
        (should (equal '("7.1c") suggestions))))))

(ert-deftest test-suggest-next-child-letter-ending ()
  "Test suggestions for letter-ending parents."
  (test-folgezettel-org-roam--with-mock-nodes '()
    (let ((suggestions (folgezettel-org-roam-suggest-next-child "7.1a")))
      (should (equal '("7.1a1") suggestions)))))

(ert-deftest test-suggest-next-child-letter-ending-with-existing ()
  "Test suggestions for letter-ending with existing number children."
  (let* ((child1 (test-folgezettel-org-roam--create-mock-node "id1" "7.1a1 First" "/tmp/7-1a1.org"))
         (child2 (test-folgezettel-org-roam--create-mock-node "id2" "7.1a2 Second" "/tmp/7-1a2.org"))
         (nodes (list child1 child2)))
    (test-folgezettel-org-roam--with-mock-nodes nodes
      (let ((suggestions (folgezettel-org-roam-suggest-next-child "7.1a")))
        (should (equal '("7.1a3") suggestions))))))

(ert-deftest test-suggest-next-child-extended-letters ()
  "Test suggestions handle extended letter sequences (z -> aa)."
  (let* ((children (cl-loop for i from 1 to 26
                            for letter = (char-to-string (+ ?a (1- i)))
                            collect (test-folgezettel-org-roam--create-mock-node
                                     (format "id%d" i)
                                     (format "7.1%s Child" letter)
                                     (format "/tmp/7-1%s.org" letter))))
         (nodes children))
    (test-folgezettel-org-roam--with-mock-nodes nodes
      (let ((suggestions (folgezettel-org-roam-suggest-next-child "7.1")))
        (should (equal '("7.1aa") suggestions))))))

(ert-deftest test-suggest-next-child-invalid-parent ()
  "Test that invalid parent returns nil."
  (test-folgezettel-org-roam--with-mock-nodes '()
    (should (equal nil (folgezettel-org-roam-suggest-next-child "1.2.3")))
    (should (equal nil (folgezettel-org-roam-suggest-next-child "")))))

;;; ============================================================================
;;; Unit Tests: folgezettel-org-roam--find-parent-node
;;; ============================================================================

(ert-deftest test-find-parent-node-exists ()
  "Test finding a parent node that exists."
  (let* ((parent (test-folgezettel-org-roam--create-mock-node "parent-id" "7 Parent Topic" "/tmp/7.org"))
         (nodes (list parent)))
    (test-folgezettel-org-roam--with-mock-nodes nodes
      (let ((result (folgezettel-org-roam--find-parent-node "7")))
        (should result)
        (should (equal "parent-id" (org-roam-node-id result)))))))

(ert-deftest test-find-parent-node-not-exists ()
  "Test finding a parent node that does not exist."
  (let* ((other (test-folgezettel-org-roam--create-mock-node "other-id" "8 Other Topic" "/tmp/8.org"))
         (nodes (list other)))
    (test-folgezettel-org-roam--with-mock-nodes nodes
      (let ((result (folgezettel-org-roam--find-parent-node "7")))
        (should (equal nil result))))))

(ert-deftest test-find-parent-node-multiple-nodes ()
  "Test finding parent among multiple nodes."
  (let* ((node1 (test-folgezettel-org-roam--create-mock-node "id1" "1 First" "/tmp/1.org"))
         (node2 (test-folgezettel-org-roam--create-mock-node "id2" "1.2 Second" "/tmp/1-2.org"))
         (node3 (test-folgezettel-org-roam--create-mock-node "id3" "1.2a Third" "/tmp/1-2a.org"))
         (nodes (list node1 node2 node3)))
    (test-folgezettel-org-roam--with-mock-nodes nodes
      (should (equal "id2" (org-roam-node-id (folgezettel-org-roam--find-parent-node "1.2"))))
      (should (equal "id1" (org-roam-node-id (folgezettel-org-roam--find-parent-node "1")))))))

(ert-deftest test-find-parent-node-nil-address ()
  "Test finding parent with nil address returns nil."
  (test-folgezettel-org-roam--with-mock-nodes '()
    (should (equal nil (folgezettel-org-roam--find-parent-node nil)))))

;;; ============================================================================
;;; Unit Tests: folgezettel-org-roam--index-exists-p
;;; ============================================================================

(ert-deftest test-index-exists-p-found ()
  "Test that existing index is found."
  (let* ((node (test-folgezettel-org-roam--create-mock-node "id1" "1.2a Topic" "/tmp/1-2a.org"))
         (nodes (list node)))
    (test-folgezettel-org-roam--with-mock-nodes nodes
      (should (folgezettel-org-roam--index-exists-p "1.2a")))))

(ert-deftest test-index-exists-p-not-found ()
  "Test that non-existing index is not found."
  (let* ((node (test-folgezettel-org-roam--create-mock-node "id1" "1.2a Topic" "/tmp/1-2a.org"))
         (nodes (list node)))
    (test-folgezettel-org-roam--with-mock-nodes nodes
      (should (equal nil (folgezettel-org-roam--index-exists-p "1.2b"))))))

;;; ============================================================================
;;; Integration Tests: Link Insertion Functions
;;; ============================================================================

(ert-deftest test-insert-backlink-creates-heading ()
  "Test that insert-backlink creates the Parent Note heading."
  (test-folgezettel-org-roam--with-temp-dir
    (let* ((child-file (test-folgezettel-org-roam--create-temp-org-file
                        "child.org"
                        "#+title: 1.2a Child Topic\n\nContent here."))
           (parent-node (test-folgezettel-org-roam--create-mock-node
                         "parent-id" "1.2 Parent Topic" "/tmp/parent.org"))
           (folgezettel-org-roam-backlink-heading "Parent Note"))
      (with-current-buffer (find-file-noselect child-file)
        (folgezettel-org-roam--insert-backlink parent-node)
        (goto-char (point-min))
        (should (search-forward "* Parent Note" nil t))
        (should (search-forward "[[id:parent-id]" nil t))
        (kill-buffer)))))

(ert-deftest test-insert-backlink-uses-description ()
  "Test that insert-backlink uses the configured description."
  (test-folgezettel-org-roam--with-temp-dir
    (let* ((child-file (test-folgezettel-org-roam--create-temp-org-file
                        "child.org"
                        "#+title: 1.2a Child Topic\n"))
           (parent-node (test-folgezettel-org-roam--create-mock-node
                         "parent-id" "1.2 Parent Topic" "/tmp/parent.org"))
           (folgezettel-org-roam-parent-link-description "Parent note"))
      (with-current-buffer (find-file-noselect child-file)
        (folgezettel-org-roam--insert-backlink parent-node)
        (goto-char (point-min))
        (should (search-forward "[[id:parent-id][Parent note]]" nil t))
        (kill-buffer)))))

(ert-deftest test-insert-forward-link-creates-heading ()
  "Test that insert-forward-link creates the Child Notes heading."
  (test-folgezettel-org-roam--with-temp-dir
    (let* ((parent-file (test-folgezettel-org-roam--create-temp-org-file
                         "parent.org"
                         "#+title: 1.2 Parent Topic\n\nContent here."))
           (child-node (test-folgezettel-org-roam--create-mock-node
                        "child-id" "1.2a Child Topic" parent-file))
           (folgezettel-org-roam-forward-link-heading "Child Notes"))
      ;; Mock org-roam-db-update-file to avoid database errors
      (cl-letf (((symbol-function 'org-roam-db-update-file) #'ignore))
        (folgezettel-org-roam--insert-forward-link child-node parent-file)
        (with-current-buffer (find-file-noselect parent-file)
          (goto-char (point-min))
          (should (search-forward "* Child Notes" nil t))
          (should (search-forward "[[id:child-id]" nil t))
          (kill-buffer))))))

(ert-deftest test-insert-forward-link-uses-title ()
  "Test that insert-forward-link uses child title as description."
  (test-folgezettel-org-roam--with-temp-dir
    (let* ((parent-file (test-folgezettel-org-roam--create-temp-org-file
                         "parent.org"
                         "#+title: 1.2 Parent Topic\n"))
           (child-node (test-folgezettel-org-roam--create-mock-node
                        "child-id" "1.2a Child Topic" parent-file))
           (folgezettel-org-roam-child-link-description nil))
      (cl-letf (((symbol-function 'org-roam-db-update-file) #'ignore))
        (folgezettel-org-roam--insert-forward-link child-node parent-file)
        (with-current-buffer (find-file-noselect parent-file)
          (goto-char (point-min))
          (should (search-forward "[[id:child-id][1.2a Child Topic]]" nil t))
          (kill-buffer))))))

(ert-deftest test-insert-crosslink-creates-heading ()
  "Test that insert-crosslink creates the Cross References heading."
  (test-folgezettel-org-roam--with-temp-dir
    (let* ((target-file (test-folgezettel-org-roam--create-temp-org-file
                         "target.org"
                         "#+title: Target Note\n\nContent."))
           (source-node (test-folgezettel-org-roam--create-mock-node
                         "source-id" "Source Note" "/tmp/source.org"))
           (target-node (test-folgezettel-org-roam--create-mock-node
                         "target-id" "Target Note" target-file))
           (folgezettel-org-roam-crosslink-heading "Cross References"))
      (cl-letf (((symbol-function 'org-roam-db-update-file) #'ignore))
        (folgezettel-org-roam--insert-crosslink target-node source-node target-file)
        (with-current-buffer (find-file-noselect target-file)
          (goto-char (point-min))
          (should (search-forward "* Cross References" nil t))
          (should (search-forward "[[id:source-id][Source Note]]" nil t))
          (kill-buffer))))))

(ert-deftest test-insert-crosslink-prevents-duplicates ()
  "Test that insert-crosslink does not create duplicate links."
  (test-folgezettel-org-roam--with-temp-dir
    (let* ((target-file (test-folgezettel-org-roam--create-temp-org-file
                         "target.org"
                         "#+title: Target Note\n\n* Cross References\n[[id:source-id][Source Note]]\n"))
           (source-node (test-folgezettel-org-roam--create-mock-node
                         "source-id" "Source Note" "/tmp/source.org"))
           (target-node (test-folgezettel-org-roam--create-mock-node
                         "target-id" "Target Note" target-file)))
      (cl-letf (((symbol-function 'org-roam-db-update-file) #'ignore))
        (folgezettel-org-roam--insert-crosslink target-node source-node target-file)
        (with-current-buffer (find-file-noselect target-file)
          (goto-char (point-min))
          ;; Count occurrences - should be exactly 1
          (let ((count 0))
            (while (search-forward "[[id:source-id]" nil t)
              (setq count (1+ count)))
            (should (equal 1 count)))
          (kill-buffer))))))

;;; ============================================================================
;;; Integration Tests: Bidirectional Link Creation
;;; ============================================================================

(ert-deftest test-bidirectional-links-in-parent-and-child ()
  "Test that both parent and child get appropriate links."
  (test-folgezettel-org-roam--with-temp-dir
    (let* ((parent-file (test-folgezettel-org-roam--create-temp-org-file
                         "parent.org"
                         "#+title: 7.1 Parent Topic\n\nParent content."))
           (child-file (test-folgezettel-org-roam--create-temp-org-file
                        "child.org"
                        "#+title: 7.1a Child Topic\n\nChild content."))
           (parent-node (test-folgezettel-org-roam--create-mock-node
                         "parent-id" "7.1 Parent Topic" parent-file))
           (child-node (test-folgezettel-org-roam--create-mock-node
                        "child-id" "7.1a Child Topic" child-file)))
      (cl-letf (((symbol-function 'org-roam-db-update-file) #'ignore))
        ;; Insert backlink in child
        (with-current-buffer (find-file-noselect child-file)
          (folgezettel-org-roam--insert-backlink parent-node)
          (save-buffer))
        ;; Insert forward link in parent
        (folgezettel-org-roam--insert-forward-link child-node parent-file)
        
        ;; Verify child has parent link
        (with-current-buffer (find-file-noselect child-file)
          (goto-char (point-min))
          (should (search-forward "* Parent Note" nil t))
          (should (search-forward "[[id:parent-id]" nil t))
          (kill-buffer))
        
        ;; Verify parent has child link
        (with-current-buffer (find-file-noselect parent-file)
          (goto-char (point-min))
          (should (search-forward "* Child Notes" nil t))
          (should (search-forward "[[id:child-id]" nil t))
          (kill-buffer))))))

;;; ============================================================================
;;; Integration Tests: Mode Activation
;;; ============================================================================

(ert-deftest test-mode-activation ()
  "Test that mode can be activated and deactivated."
  (unwind-protect
      (progn
        ;; Activate mode
        (folgezettel-org-roam-mode 1)
        (should (equal t folgezettel-org-roam-mode))
        (should (member 'folgezettel-org-roam--process-new-node 
                        org-roam-capture-new-node-hook))
        
        ;; Deactivate mode
        (folgezettel-org-roam-mode -1)
        (should (equal nil folgezettel-org-roam-mode))
        (should (not (member 'folgezettel-org-roam--process-new-node 
                             org-roam-capture-new-node-hook))))
    ;; Cleanup
    (folgezettel-org-roam-mode -1)))

;;; ============================================================================
;;; Regression Tests
;;; ============================================================================

(ert-deftest test-regression-multidigit-numbers ()
  "Regression: Multi-digit numbers should be handled correctly."
  (should (equal "1" (folgezettel-org-roam--parse-address "1.123")))
  (should (equal "1.123" (folgezettel-org-roam--parse-address "1.123a")))
  (should (equal "1.123a" (folgezettel-org-roam--parse-address "1.123a456"))))

(ert-deftest test-regression-extended-alphabet-parent ()
  "Regression: Extended alphabet sequences should parse to correct parent."
  ;; aa, ab, etc. are children of 1.2, not 1.2a
  (should (equal "1.2" (folgezettel-org-roam--parse-address "1.2aa")))
  (should (equal "1.2" (folgezettel-org-roam--parse-address "1.2ab")))
  (should (equal "1.2" (folgezettel-org-roam--parse-address "1.2zz")))
  (should (equal "1.2" (folgezettel-org-roam--parse-address "1.2aaa"))))

(ert-deftest test-regression-deep-chains ()
  "Regression: Very deep chains should be handled."
  (should (equal "1.2a3b4c5d6e7f8g9h"
                 (folgezettel-org-roam--parse-address "1.2a3b4c5d6e7f8g9h10")))
  (should (equal "1.2a3b4c5d6e7f8g9h10"
                 (folgezettel-org-roam--parse-address "1.2a3b4c5d6e7f8g9h10i"))))

(ert-deftest test-regression-title-with-numbers ()
  "Regression: Titles containing numbers should extract correctly."
  (should (equal "1.2" (folgezettel-org-roam--extract-from-title "1.2 Chapter 5 Analysis")))
  (should (equal "3.14" (folgezettel-org-roam--extract-from-title "3.14 Pi Discussion"))))

(ert-deftest test-regression-case-sensitivity ()
  "Regression: Uppercase letters should be rejected."
  (should (stringp (folgezettel-org-roam--validate-no-invalid-characters "1.2A")))
  (should (stringp (folgezettel-org-roam--validate-no-invalid-characters "1.2aB3")))
  (should (equal nil (folgezettel-org-roam-validate-address "1.2A"))))

;;; ============================================================================
;;; Edge Case Tests
;;; ============================================================================

(ert-deftest test-edge-empty-title ()
  "Test handling of empty titles."
  (should (equal nil (folgezettel-org-roam--extract-from-title "")))
  (should (equal nil (folgezettel-org-roam--extract-from-title nil))))

(ert-deftest test-edge-whitespace-title ()
  "Test handling of whitespace-only titles."
  (should (equal nil (folgezettel-org-roam--extract-from-title "   ")))
  (should (equal nil (folgezettel-org-roam--extract-from-title "\t\n"))))

(ert-deftest test-edge-very-long-address ()
  "Test handling of very long addresses."
  (let ((long-addr "1.2a3b4c5d6e7f8g9h10i11j12k13l14m15n16o17p18q19r20s21t22u23v24w25x26y27z28"))
    (should (folgezettel-org-roam-validate-address long-addr))))

(ert-deftest test-edge-single-letter-all ()
  "Test all single letters work correctly."
  (dolist (letter (mapcar #'char-to-string (number-sequence ?a ?z)))
    (let ((addr (concat "1.2" letter)))
      (should (equal "1.2" (folgezettel-org-roam--parse-address addr))))))

(ert-deftest test-edge-customization-nil-heading ()
  "Test behavior when headings are set to nil."
  (test-folgezettel-org-roam--with-temp-dir
    (let* ((child-file (test-folgezettel-org-roam--create-temp-org-file
                        "child.org"
                        "#+title: 1.2a Child Topic\n\n"))
           (parent-node (test-folgezettel-org-roam--create-mock-node
                         "parent-id" "1.2 Parent Topic" "/tmp/parent.org"))
           (folgezettel-org-roam-backlink-heading nil))
      (with-current-buffer (find-file-noselect child-file)
        (folgezettel-org-roam--insert-backlink parent-node)
        (goto-char (point-min))
        ;; Should not have a heading
        (should (not (search-forward "* Parent Note" nil t)))
        ;; But should have the link
        (goto-char (point-min))
        (should (search-forward "[[id:parent-id]" nil t))
        (kill-buffer)))))

;;; ============================================================================
;;; Performance Tests (Optional - can be slow)
;;; ============================================================================

(ert-deftest test-perf-many-nodes-lookup ()
  "Test performance with many nodes (should complete quickly)."
  :tags '(:perf)
  (let* ((nodes (cl-loop for i from 1 to 1000
                         collect (test-folgezettel-org-roam--create-mock-node
                                  (format "id%d" i)
                                  (format "1.%d Topic %d" i i)
                                  (format "/tmp/1-%d.org" i)))))
    (test-folgezettel-org-roam--with-mock-nodes nodes
      (let ((start-time (float-time)))
        (folgezettel-org-roam--find-parent-node "1.500")
        (let ((elapsed (- (float-time) start-time)))
          ;; Should complete in under 1 second
          (should (< elapsed 1.0)))))))

(ert-deftest test-perf-validation-batch ()
  "Test validation performance on many addresses."
  :tags '(:perf)
  (let ((addresses '("1" "1.2" "1.2a" "1.2a3" "1.2a3b" "1.2a3b4"
                     "1.13" "1.13aa" "1.13aa5" "42.99zz15abc")))
    (let ((start-time (float-time)))
      (dotimes (_ 1000)
        (dolist (addr addresses)
          (folgezettel-org-roam-validate-address addr)))
      (let ((elapsed (- (float-time) start-time)))
        ;; 10000 validations should complete in under 2 seconds
        (should (< elapsed 2.0))))))

;;; ============================================================================
;;; Test Runner Helper
;;; ============================================================================

(defun test-folgezettel-org-roam-run-all ()
  "Run all folgezettel-org-roam tests."
  (interactive)
  (ert-run-tests-interactively "^test-"))

(defun test-folgezettel-org-roam-run-unit ()
  "Run only unit tests (fast)."
  (interactive)
  (ert-run-tests-interactively "^test-\\(parse\\|extract\\|next-letter\\|validate\\|suggest\\|find\\|index\\)"))

(defun test-folgezettel-org-roam-run-integration ()
  "Run only integration tests."
  (interactive)
  (ert-run-tests-interactively "^test-\\(insert\\|bidirectional\\|mode\\)"))

(provide 'test-folgezettel-org-roam)

;;; test-folgezettel-org-roam.el ends here
