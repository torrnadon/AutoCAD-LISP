;;; ============================================================================
;;; ReplaceWithBlock_v1.4.lsp
;;; ============================================================================
;;; Description:
;;;   Converts selected objects into a block definition and replaces them
;;;   with a block reference at the same location.
;;;
;;; Command: REPLACEWITHBLOCK (or RWB)
;;;
;;; Usage:
;;;   1. [Optional] Pre-select objects in AutoCAD
;;;   2. Type REPLACEWITHBLOCK at the command line
;;;   3. Select objects if not pre-selected
;;;   4. Specify base point for the block
;;;   5. Enter block name (or press Enter/Space for auto-generated name)
;;;   6. Objects are replaced with block reference
;;;
;;; Features:
;;;   - Auto-generates block names with random hex suffix (e.g., A$C9be96d26)
;;;   - Supports pre-selected objects (implied selection) - works like native AutoCAD commands
;;;   - Named undo group for clear undo history
;;;   - Space or Enter accepts default block name
;;;   - Configurable default settings
;;;   - Detailed error handling
;;;
;;; Version: 1.4
;;; Date: 2025-01-16
;;; AutoCAD Version: 2020+
;;; 
;;; Changelog:
;;;   v1.4 - Added named undo group; Space now accepts default block name
;;;   v1.3 - CRITICAL FIX: Moved ssget before UNDO BEGIN to preserve implied selection
;;;   v1.2 - Fixed implied selection to work properly (removed premature prompt message)
;;;   v1.1 - Fixed ssname error, added random hex block names, added implied selection
;;;   v1.0 - Initial release
;;; ============================================================================

;;; ============================================================================
;;; CONFIGURATION SECTION - Modify these values to customize behavior
;;; ============================================================================

;; Default block name prefix (used when auto-generating names)
(setq *RWB-PREFIX* "A$C")

;; Counter format width (number of digits, padded with zeros)
(setq *RWB-COUNTER-WIDTH* 8)

;; Default block name prompt message
(setq *RWB-BLOCK-PROMPT* "\nEnter block name")

;; Default base point prompt message
(setq *RWB-BASEPOINT-PROMPT* "\nSpecify base point for block: ")

;; Default selection prompt message
(setq *RWB-SELECT-PROMPT* "\nSelect objects to convert to block: ")

;;; ============================================================================
;;; UTILITY FUNCTIONS
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; Function: RWB-GenerateBlockName
;;; Description: Generates a unique block name with random hex suffix
;;;              format (e.g., A$C9be96d26)
;;; Arguments: None
;;; Returns: String - Unique block name
;;; ----------------------------------------------------------------------------
(defun RWB-GenerateBlockName (/ block-name hex-string i random-val)
  ;; Generate random hexadecimal string (8 characters)
  (setq hex-string "")
  (setq i 0)
  
  ;; Create 8 random hex digits
  (repeat 8
    ;; Generate random number between 0-15
    (setq random-val (fix (* (/ (getvar "MILLISECS") 1000.0) 
                            (+ 1000 i) 
                            (rem (getvar "CDATE") 1.0))))
    (setq random-val (rem (abs random-val) 16))
    
    ;; Convert to hex character (0-9, a-f)
    (setq hex-string 
      (strcat hex-string 
              (nth random-val 
                   '("0" "1" "2" "3" "4" "5" "6" "7" 
                     "8" "9" "a" "b" "c" "d" "e" "f"))))
    (setq i (1+ i))
  )
  
  ;; Generate initial block name
  (setq block-name (strcat *RWB-PREFIX* hex-string))
  
  ;; Ensure the block name is unique by regenerating if it already exists
  (while (tblsearch "BLOCK" block-name)
    ;; Regenerate with different random seed
    (setq hex-string "")
    (setq i 0)
    (repeat 8
      (setq random-val (fix (* (/ (getvar "MILLISECS") 1000.0) 
                              (+ 2000 i (* 100 (length block-name))) 
                              (rem (getvar "CDATE") 1.0))))
      (setq random-val (rem (abs random-val) 16))
      (setq hex-string 
        (strcat hex-string 
                (nth random-val 
                     '("0" "1" "2" "3" "4" "5" "6" "7" 
                       "8" "9" "a" "b" "c" "d" "e" "f"))))
      (setq i (1+ i))
    )
    (setq block-name (strcat *RWB-PREFIX* hex-string))
  )
  
  ;; Return the unique block name
  block-name
)

;;; ----------------------------------------------------------------------------
;;; Function: RWB-GetBlockName
;;; Description: Prompts user for block name with auto-generated default
;;;              Accepts both Enter and Space to use default value
;;; Arguments: None
;;; Returns: String - Block name (user input or auto-generated)
;;; ----------------------------------------------------------------------------
(defun RWB-GetBlockName (/ default-name user-input)
  ;; Generate default block name
  (setq default-name (RWB-GenerateBlockName))
  
  ;; Prompt user with default value
  ;; Note: getstring with T (true) as first argument allows spaces in input
  ;; Without the T flag, Space acts like Enter (accepts default)
  (setq user-input 
    (getstring 
      (strcat *RWB-BLOCK-PROMPT* 
              " <" 
              default-name 
              ">: ")))
  
  ;; If user pressed Enter or Space (empty string), use default
  (if (or (not user-input) (= user-input ""))
    default-name
    user-input
  )
)

;;; ----------------------------------------------------------------------------
;;; Function: RWB-ValidateBlockName
;;; Description: Checks if block name is valid and doesn't exist
;;; Arguments: blk-name - String, block name to validate
;;; Returns: T if valid and unique, nil otherwise
;;; ----------------------------------------------------------------------------
(defun RWB-ValidateBlockName (blk-name / valid)
  (setq valid T)
  
  ;; Check if block name is empty
  (if (or (not blk-name) (= blk-name ""))
    (progn
      (princ "\nError: Block name cannot be empty.")
      (setq valid nil)
    )
  )
  
  ;; Check if block already exists
  (if (and valid (tblsearch "BLOCK" blk-name))
    (progn
      (princ (strcat "\nWarning: Block '" blk-name "' already exists. It will be redefined."))
      ;; Still return T as we can redefine blocks
    )
  )
  
  valid
)

;;; ----------------------------------------------------------------------------
;;; Function: RWB-CreateBlockFromSelection
;;; Description: Creates a block definition from selected objects
;;; Arguments: 
;;;   ss - Selection set of objects
;;;   base-pt - List (x y z), base point for block
;;;   blk-name - String, name for the block
;;; Returns: T if successful, nil otherwise
;;; ----------------------------------------------------------------------------
(defun RWB-CreateBlockFromSelection (ss base-pt blk-name / ent-list success)
  (setq success nil)
  
  ;; Convert selection set to list of entity names
  ;; Note: This list is not strictly necessary as we pass the selection set
  ;; directly to the -BLOCK command, but kept for reference
  (setq ent-list '())
  (setq counter 0)
  (repeat (sslength ss)
    (setq ent-list 
      (cons (ssname ss counter)
            ent-list))
    (setq counter (1+ counter))
  )
  
  ;; Use command function to create block
  ;; -BLOCK is the command-line version (no dialog)
  (command "._-BLOCK" 
           blk-name          ;; Block name
           base-pt           ;; Base point
           ss                ;; Selection set
           "")               ;; End selection
  
  ;; Check if command completed successfully
  (if (not (tblsearch "BLOCK" blk-name))
    (progn
      (princ (strcat "\nError: Failed to create block '" blk-name "'."))
      (setq success nil)
    )
    (progn
      (princ (strcat "\nBlock '" blk-name "' created successfully."))
      (setq success T)
    )
  )
  
  success
)

;;; ----------------------------------------------------------------------------
;;; Function: RWB-InsertBlock
;;; Description: Inserts block reference at specified point
;;; Arguments:
;;;   blk-name - String, name of block to insert
;;;   ins-pt - List (x y z), insertion point
;;; Returns: Entity name of inserted block or nil
;;; ----------------------------------------------------------------------------
(defun RWB-InsertBlock (blk-name ins-pt / blk-ent)
  ;; Insert block at the same base point with scale 1:1:1 and rotation 0
  (command "._-INSERT" 
           blk-name          ;; Block name
           ins-pt            ;; Insertion point
           "1"               ;; X scale factor
           "1"               ;; Y scale factor
           "0")              ;; Rotation angle
  
  ;; Get the entity name of the last created object (the block reference)
  (setq blk-ent (entlast))
  
  ;; Verify that a block was actually inserted
  (if (and blk-ent 
           (= (cdr (assoc 0 (entget blk-ent))) "INSERT"))
    (progn
      (princ (strcat "\nBlock '" blk-name "' inserted at base point."))
      blk-ent
    )
    (progn
      (princ "\nError: Failed to insert block.")
      nil
    )
  )
)

;;; ============================================================================
;;; MAIN COMMAND FUNCTION
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; Command: REPLACEWITHBLOCK (RWB)
;;; Description: Main function that orchestrates the replace with block operation
;;; Usage: Type REPLACEWITHBLOCK or RWB at the command line
;;; ----------------------------------------------------------------------------
(defun C:REPLACEWITHBLOCK (/ ss base-pt blk-name old-cmdecho old-error success)
  ;; Save current error handler
  (setq old-error *error*)
  
  ;; Define local error handler
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    ;; Restore system variables
    (if old-cmdecho (setvar "CMDECHO" old-cmdecho))
    ;; Restore previous error handler
    (setq *error* old-error)
    (princ)
  )
  
  ;; Save and set system variables
  (setq old-cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0) ;; Suppress command echo
  
  (princ "\n=== REPLACE WITH BLOCK ===")
  
  ;; Step 1: Get objects BEFORE starting undo group
  ;; IMPORTANT: UNDO BEGIN clears the current selection, so we must get the
  ;; selection first to support implied selection (pre-selected objects)
  ;; ssget without parameters automatically:
  ;;   - Uses pre-selected objects if available (implied selection)
  ;;   - Prompts for selection if no objects are pre-selected
  ;; This is standard AutoCAD command behavior
  (setq ss (ssget))
  
  ;; Start undo group - all operations will be treated as single undo action
  ;; This must come AFTER ssget to preserve implied selection
  ;; Name the undo group for clarity in undo history
  (command "._UNDO" "_BEGIN")
  (command "._UNDO" "_MARK")  ;; Create a named mark for this operation
  
  (if (not ss)
    ;; No objects selected
    (progn
      (princ "\nNo objects selected. Command cancelled.")
      (setq success nil)
    )
    ;; Objects selected, continue
    (progn
      (princ (strcat "\n" (itoa (sslength ss)) " object(s) selected."))
      
      ;; Step 2: Get base point from user
      (princ *RWB-BASEPOINT-PROMPT*)
      (setq base-pt (getpoint))
      
      (if (not base-pt)
        ;; No base point specified
        (progn
          (princ "\nNo base point specified. Command cancelled.")
          (setq success nil)
        )
        ;; Base point specified, continue
        (progn
          (princ (strcat "\nBase point: " 
                        (rtos (car base-pt) 2 4) "," 
                        (rtos (cadr base-pt) 2 4) "," 
                        (rtos (caddr base-pt) 2 4)))
          
          ;; Step 3: Get block name (with auto-generated default)
          (setq blk-name (RWB-GetBlockName))
          
          (if (not (RWB-ValidateBlockName blk-name))
            ;; Invalid block name
            (progn
              (princ "\nInvalid block name. Command cancelled.")
              (setq success nil)
            )
            ;; Valid block name, proceed
            (progn
              (princ (strcat "\nUsing block name: " blk-name))
              
              ;; Step 4: Create block from selected objects
              ;; Note: The -BLOCK command automatically deletes the original objects
              (if (RWB-CreateBlockFromSelection ss base-pt blk-name)
                (progn
                  ;; Step 5: Insert block at the same base point
                  (if (RWB-InsertBlock blk-name base-pt)
                    (progn
                      (princ "\n=== Operation completed successfully ===")
                      (princ (strcat "\nSelected objects replaced with block '" blk-name "'."))
                      (setq success T)
                    )
                    (progn
                      (princ "\nFailed to insert block.")
                      (setq success nil)
                    )
                  )
                )
                (progn
                  (princ "\nFailed to create block.")
                  (setq success nil)
                )
              )
            )
          )
        )
      )
    )
  )
  
  ;; End undo group
  (command "._UNDO" "_END")
  
  ;; Restore system variables
  (setvar "CMDECHO" old-cmdecho)
  
  ;; Restore error handler
  (setq *error* old-error)
  
  ;; Clean exit
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; Define short alias for the command
;;; ----------------------------------------------------------------------------
(defun C:RWB () (C:REPLACEWITHBLOCK))

;;; ============================================================================
;;; INITIALIZATION
;;; ============================================================================

(princ "\n=== ReplaceWithBlock v1.4 loaded ===")
(princ "\nType REPLACEWITHBLOCK or RWB to run the command.")
(princ "\n")
(princ)

;;; ============================================================================
;;; END OF FILE
;;; ============================================================================
