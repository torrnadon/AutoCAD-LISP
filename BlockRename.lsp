(defun c:BLOCKRENAME (/ ss ss-count user-continue new-base-name block-exists counter 
                     ent ent-data old-block-name new-block-name suffix 
                     blocks-to-rename unique-blocks block-list temp-name blocks-count
                     user-response name-conflict default-block-name)
  
  ;; ========================================================================
  ;; RENAME BLOCKS COMMAND - BLOCKRENAME
  ;; ========================================================================
  ;; This script renames selected block definitions in AutoCAD
  ;; Features:
  ;; - Handles single or multiple block selections
  ;; - Asks for confirmation when multiple blocks are selected
  ;; - Checks if new block name already exists
  ;; - Adds numeric suffixes (_001, _002, etc.) for multiple blocks
  ;; - Renames the actual block definition, not just references
  ;; ========================================================================
  
  (princ "\n=== Rename Blocks ===")
  
  ;; ========================================================================
  ;; STEP 1: GET USER SELECTION OF BLOCKS
  ;; ========================================================================
  
  ;; Prompt user to select blocks, filter only INSERT entities (block references)
  (setq ss (ssget '((0 . "INSERT"))))
  
  ;; Check if user made a selection
  (if ss
    (progn
      ;; Get the count of selected blocks
      (setq ss-count (sslength ss))
      
      ;; ====================================================================
      ;; STEP 2: COLLECT UNIQUE BLOCK NAMES FROM SELECTION
      ;; ====================================================================
      
      ;; Initialize list to store unique block names
      (setq unique-blocks '())
      
      ;; Loop through selection to find unique block names
      (setq counter 0)
      (repeat ss-count
        ;; Get entity name from selection set
        (setq ent (ssname ss counter))
        ;; Get entity data
        (setq ent-data (entget ent))
        ;; Extract block name (DXF code 2)
        (setq old-block-name (cdr (assoc 2 ent-data)))
        
        ;; Add to list if not already present
        ;; member checks if item exists in list
        (if (not (member old-block-name unique-blocks))
          (setq unique-blocks (cons old-block-name unique-blocks))
        )
        
        (setq counter (1+ counter))
      )
      
      ;; Reverse the list to maintain selection order
      (setq unique-blocks (reverse unique-blocks))
      
      ;; Get count of unique blocks
      (setq blocks-count (length unique-blocks))
      
      ;; Display the number of unique blocks found
      (princ (strcat "\nSelected: " (itoa ss-count) " block reference(s)"))
      (princ (strcat "\nUnique block definition(s): " (itoa blocks-count)))
      
      ;; ====================================================================
      ;; STEP 3: CONFIRMATION FOR MULTIPLE BLOCKS
      ;; ====================================================================
      
      ;; Initialize continue flag as true
      (setq user-continue T)
      
      ;; If more than one unique block is selected, ask for confirmation
      (if (> blocks-count 1)
        (progn
          (initget "Yes No")
          ;; Ask user to continue, default is "Yes"
          (setq user-response 
                (getkword "\nMultiple block definitions selected. Suffix _XXX will be added. Continue? [Yes/No] <Yes>: "))
          
          ;; If user selected "No", set continue flag to nil
          (if (= user-response "No")
            (setq user-continue nil)
          )
        )
      )
      
      ;; ====================================================================
      ;; STEP 3.5: CHECK IF BLOCK COUNT IS WITHIN SUFFIX FORMAT LIMIT
      ;; ====================================================================
      
      ;; If multiple blocks and user wants to continue, check the count limit
      (if (and user-continue (> blocks-count 1))
        (progn
          ;; Check if block count exceeds 999 (suffix format limit _001 to _999)
          (if (> blocks-count 999)
            (progn
              (princ (strcat "\nError: Selected " 
                            (itoa blocks-count) 
                            " block definitions."))
              (princ "\nCurrent suffix format (_001 to _999) supports maximum 999 blocks.")
              (princ "\nPlease select fewer blocks or modify the suffix format.")
              ;; Cancel operation
              (setq user-continue nil)
            )
          )
        )
      )
      
      ;; ====================================================================
      ;; STEP 4: GET NEW BLOCK NAME FROM USER
      ;; ====================================================================
      
      (if user-continue
        (progn
          ;; Get the first block name from the list as default suggestion
          (setq default-block-name (car unique-blocks))
          
          ;; Prompt user to enter new block name with default shown
          (setq new-base-name 
                (getstring T 
                          (strcat "\nEnter new block name <" 
                                  default-block-name 
                                  ">: ")))
          
          ;; If user pressed Enter (empty string), use the default name
          (if (or (= new-base-name "") (= new-base-name nil))
            (setq new-base-name default-block-name)
          )
          
          ;; Check if user entered a name (not empty or cancelled)
          (if (and new-base-name (/= new-base-name ""))
            (progn
              
              ;; ============================================================
              ;; STEP 5: CHECK IF BLOCK NAME ALREADY EXISTS
              ;; ============================================================
              
              ;; For single block: check if exact name exists
              ;; For multiple blocks: check if any suffixed names exist
              (setq name-conflict nil)
              
              (if (= blocks-count 1)
                ;; Check single name
                (progn
                  (setq block-exists (tblsearch "BLOCK" new-base-name))
                  ;; Only conflict if it's not renaming to itself
                  (if (and block-exists 
                           (/= new-base-name (car unique-blocks)))
                    (setq name-conflict T)
                  )
                )
                ;; Check if any suffixed names exist (for multiple blocks)
                (progn
                  (setq counter 1)
                  (while (and (<= counter blocks-count) (not name-conflict))
                    ;; Format suffix
                    (setq suffix (strcat "_" 
                                        (substr "000" 1 
                                                (- 3 (strlen (itoa counter))))
                                        (itoa counter)))
                    (setq temp-name (strcat new-base-name suffix))
                    
                    ;; Check if this suffixed name exists
                    (if (tblsearch "BLOCK" temp-name)
                      (setq name-conflict T)
                    )
                    (setq counter (1+ counter))
                  )
                )
              )
              
              ;; If name conflict found, warn user
              (if name-conflict
                (progn
                  (princ (strcat "\nWarning: Block name '" 
                                 new-base-name 
                                 "' or its variants already exist in the drawing."))
                  
                  ;; Ask if user wants to continue anyway
                  (initget "Yes No")
                  (setq user-response 
                        (getkword "\nContinue anyway? [Yes/No] <No>: "))
                  
                  ;; If user chose "No" or pressed Enter (default), cancel
                  (if (or (= user-response "No") (= user-response nil))
                    (setq user-continue nil)
                  )
                )
              )
              
              ;; ============================================================
              ;; STEP 6: RENAME BLOCK DEFINITIONS USING COMMAND
              ;; ============================================================
              
              (if user-continue
                (progn
                  ;; Initialize counter for suffix numbering
                  (setq counter 1)
                  
                  ;; Loop through each unique block name
                  (foreach old-block-name unique-blocks
                    
                    ;; =======================================================
                    ;; DETERMINE NEW BLOCK NAME WITH OR WITHOUT SUFFIX
                    ;; =======================================================
                    
                    (if (> blocks-count 1)
                      ;; For multiple blocks: add suffix _001, _002, etc.
                      (progn
                        ;; Format counter as 3-digit number with leading zeros
                        (setq suffix (strcat "_" 
                                            (substr "000" 1 
                                                    (- 3 (strlen (itoa counter))))
                                            (itoa counter)))
                        
                        ;; Combine base name with suffix
                        (setq new-block-name (strcat new-base-name suffix))
                      )
                      ;; For single block: use base name without suffix
                      (setq new-block-name new-base-name)
                    )
                    
                    ;; =======================================================
                    ;; USE RENAME COMMAND TO RENAME BLOCK DEFINITION
                    ;; =======================================================
                    
                    ;; Use command function to execute AutoCAD RENAME command
                    ;; This properly renames the block definition in the block table
                    (command "._RENAME" "BLOCK" old-block-name new-block-name)
                    
                    ;; Check if rename was successful
                    (if (tblsearch "BLOCK" new-block-name)
                      (princ (strcat "\nRenamed: " 
                                     old-block-name 
                                     " -> " 
                                     new-block-name))
                      (princ (strcat "\nError renaming: " old-block-name))
                    )
                    
                    ;; Increment counter for next block
                    (setq counter (1+ counter))
                  )
                  
                  ;; Display completion message
                  (princ (strcat "\n" (itoa blocks-count) 
                                " block definition(s) renamed successfully."))
                )
                ;; If user cancelled after name check
                (princ "\nOperation cancelled.")
              )
            )
            ;; If no name was entered
            (princ "\nNo block name entered. Operation cancelled.")
          )
        )
        ;; If user chose not to continue with multiple blocks
        (princ "\nOperation cancelled by user.")
      )
    )
    ;; If no blocks were selected
    (princ "\nNo blocks selected.")
  )
  
  ;; Clean exit - return nil to suppress extra output in command line
  (princ)
)

;; ========================================================================
;; COMMAND LOADING MESSAGE
;; ========================================================================
(princ "\nRename Blocks loaded. Type BLOCKRENAME to start.")
(princ)
