
;;;;;;;;;;;;;;;;;;; Start of KMD-350 ;;;;;;;;;;;;;;;;;;;

(defun insertBlockExplode (blockName insertPoint / acadDoc modelSpace blockRef)
  (setq acadDoc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq modelSpace (vla-get-ModelSpace acadDoc))

  ;; kiểm tra block tồn tại
  (if (not (tblsearch "BLOCK" blockName))
    (progn
      (princ (strcat "\n❌ Block không tồn tại: " blockName))
      nil
    )
    (progn
      (setq blockRef
        (vla-InsertBlock
          modelSpace
          (vlax-3d-point insertPoint)
          blockName
          1.0 1.0 1.0
          0.0
        )
      )
      (vla-Explode blockRef)
      (vla-Delete blockRef)
      T
    )
  )
)

(defun c:350 ( / selectionSet entityCount index
                  entity entityType
                  circleList
                  circleOne circleTwo
                  centerOne centerTwo
                  upperCircle lowerCircle
                  blockUpper blockLower)

  (vl-load-com)

  ;; ===== CẤU HÌNH =====
  (setq blockUpper "KMD-350")
  (setq blockLower "KMD-350-2")

  ;; ===== TẠO LAYER =====
  (if (not (tblsearch "LAYER" "_mss.dut"))
    (command "-layer" "m" "_mss.dut" "")
  )

  ;; ===== QUÉT CHỌN CỤM =====
  (setq selectionSet (ssget))
  (if (null selectionSet)
    (progn
      (princ "\n❌ Chưa chọn đối tượng.")
      (princ)
      (return)
    )
  )

  ;; ===== TÁCH CIRCLE =====
  (setq circleList '())
  (setq entityCount (sslength selectionSet))
  (setq index 0)

  (repeat entityCount
    (setq entity (ssname selectionSet index))
    (setq entityType (cdr (assoc 0 (entget entity))))

    (if (= entityType "CIRCLE")
      (setq circleList (cons entity circleList))
      (entdel entity)
    )

    (setq index (1+ index))
  )

  ;; ===== KIỂM TRA SỐ LỖ =====
  (if (/= (length circleList) 2)
    (progn
      (princ "\n❌ Phải có đúng 2 lỗ tròn trong nhóm.")
      (princ)
      (return)
    )
  )

  ;; ===== ĐỔI LAYER =====
  (foreach circleEntity circleList
    (entmod
      (subst
        (cons 8 "_mss.dut")
        (assoc 8 (entget circleEntity))
        (entget circleEntity)
      )
    )
  )

  ;; ===== PHÂN LỖ TRÊN / DƯỚI =====
  (setq circleOne (car circleList))
  (setq circleTwo (cadr circleList))

  (setq centerOne (cdr (assoc 10 (entget circleOne))))
  (setq centerTwo (cdr (assoc 10 (entget circleTwo))))

  (if (> (cadr centerOne) (cadr centerTwo))
    (progn
      (setq upperCircle circleOne)
      (setq lowerCircle circleTwo)
    )
    (progn
      (setq upperCircle circleTwo)
      (setq lowerCircle circleOne)
    )
  )

  ;; ===== DỊCH CHUYỂN =====
  (command "move" upperCircle "" '(0 0 0) '(0 -9 0))
  (command "move" lowerCircle "" '(0 0 0) '(0 -10 0))

  ;; ===== INSERT + EXPLODE =====
  (setq centerOne (cdr (assoc 10 (entget upperCircle))))
  (setq centerTwo (cdr (assoc 10 (entget lowerCircle))))

  (insertBlockExplode blockUpper centerOne)
  (command "explode" (entlast))

  (insertBlockExplode blockLower centerTwo)
  (command "explode" (entlast))
)


;;;;;;;;;;;;;;;;;;; End of 350 ;;;;;;;;;;;;;;;;;;;;;;



(defun c:535 ( / selectionSet entityCount index
                  entity entityType
                  circleList
                  circleOne circleTwo
                  centerOne centerTwo
                  upperCircle lowerCircle
                  blockUpper blockLower)

  (vl-load-com)

  ;; ===== CẤU HÌNH =====
  (setq blockUpper "KMD-535")
  (setq blockLower "KMD-535-2")

  ;; ===== TẠO LAYER =====
  (if (not (tblsearch "LAYER" "_mss.dut"))
    (command "-layer" "m" "_mss.dut" "")
  )

  ;; ===== QUÉT CHỌN CỤM =====
  (setq selectionSet (ssget))
  (if (null selectionSet)
    (progn
      (princ "\n❌ Chưa chọn đối tượng.")
      (princ)
      (return)
    )
  )

  ;; ===== TÁCH CIRCLE =====
  (setq circleList '())
  (setq entityCount (sslength selectionSet))
  (setq index 0)

  (repeat entityCount
    (setq entity (ssname selectionSet index))
    (setq entityType (cdr (assoc 0 (entget entity))))

    (if (= entityType "CIRCLE")
      (setq circleList (cons entity circleList))
      (entdel entity)
    )

    (setq index (1+ index))
  )

  ;; ===== KIỂM TRA SỐ LỖ =====
  (if (/= (length circleList) 2)
    (progn
      (princ "\n❌ Phải có đúng 2 lỗ tròn trong nhóm.")
      (princ)
      (return)
    )
  )

  ;; ===== ĐỔI LAYER =====
  (foreach circleEntity circleList
    (entmod
      (subst
        (cons 8 "_mss.dut")
        (assoc 8 (entget circleEntity))
        (entget circleEntity)
      )
    )
  )

  ;; ===== PHÂN LỖ TRÊN / DƯỚI =====
  (setq circleOne (car circleList))
  (setq circleTwo (cadr circleList))

  (setq centerOne (cdr (assoc 10 (entget circleOne))))
  (setq centerTwo (cdr (assoc 10 (entget circleTwo))))

  (if (> (cadr centerOne) (cadr centerTwo))
    (progn
      (setq upperCircle circleOne)
      (setq lowerCircle circleTwo)
    )
    (progn
      (setq upperCircle circleTwo)
      (setq lowerCircle circleOne)
    )
  )

  ;; ===== DỊCH CHUYỂN =====
  (command "move" upperCircle "" '(0 0 0) '(0 -9 0))
  (command "move" lowerCircle "" '(0 0 0) '(0 -10 0))

  ;; ===== INSERT + EXPLODE =====
  (setq centerOne (cdr (assoc 10 (entget upperCircle))))
  (setq centerTwo (cdr (assoc 10 (entget lowerCircle))))

  (insertBlockExplode blockUpper centerOne)
  (command "explode" (entlast))

  (insertBlockExplode blockLower centerTwo)
  (command "explode" (entlast))
)
