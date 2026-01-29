
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

;;;;;;;;;;;;;;;;;;; Start of 535 ;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;; End of 535 ;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;; Start of TKK ;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; Start of TKK ;;;;;;;;;;;;;;;;;;;
(defun c:TKKTHAYKHOA
  (/ selectionSetOldLock selectionSetNewLock
     circleEntityList
     firstCircle secondCircle
     firstCircleCenter secondCircleCenter
     upperHoleCircle lowerHoleCircle
     upperHoleCenter lowerHoleCenter
     newLockCenterX offsetDistance
     moveUpperHoleVector moveLowerHoleVector
     newLockCenter
     copySelectionSet
     loopIndex
     currentEntity currentEntityType currentEntityData)

  (vl-load-com)

  ;; =================================================
  ;; PHẦN 1 – XỬ LÝ KHÓA CŨ
  ;; =================================================
  (prompt "\nChon khoa cu: ")
  (setq selectionSetOldLock (ssget))
  (if (not selectionSetOldLock) (exit))

  (setq circleEntityList '())
  (setq loopIndex 0)

  ;; Tách circle trong khóa cũ
  (repeat (sslength selectionSetOldLock)
    (setq currentEntity (ssname selectionSetOldLock loopIndex))
    (setq currentEntityData (entget currentEntity))
    (setq currentEntityType (cdr (assoc 0 currentEntityData)))

    (if (= currentEntityType "CIRCLE")
      (setq circleEntityList (cons currentEntity circleEntityList))
    )

    (setq loopIndex (1+ loopIndex))
  )

  (if (/= (length circleEntityList) 2)
    (progn
      (princ "\n❌ Khoa cu phai co dung 2 CIRCLE.")
      (exit)
    )
  )
  ;; ===== XÓA LỖ KHÓA CŨ (KHÔNG PHẢI CIRCLE) =====
  (setq loopIndex 0)
  (repeat (sslength selectionSetOldLock)
    (setq currentEntity (ssname selectionSetOldLock loopIndex))
    (setq currentEntityData (entget currentEntity))
    (setq currentEntityType (cdr (assoc 0 currentEntityData)))

    (if (member currentEntityType '("LINE" "ARC" "LWPOLYLINE"))
      (entdel currentEntity)
    )

    (setq loopIndex (1+ loopIndex))
  )

  
  ;; Đổi layer 2 lỗ
  (foreach circleEntity circleEntityList
    (entmod
      (subst
        (cons 8 "_mss.dut")
        (assoc 8 (entget circleEntity))
        (entget circleEntity)
      )
    )
  )

  ;; Phân lỗ trên / dưới
  (setq firstCircle (car circleEntityList))
  (setq secondCircle (cadr circleEntityList))

  (setq firstCircleCenter (cdr (assoc 10 (entget firstCircle))))
  (setq secondCircleCenter (cdr (assoc 10 (entget secondCircle))))

  (if (> (cadr firstCircleCenter) (cadr secondCircleCenter))
    (progn
      (setq upperHoleCircle firstCircle)
      (setq lowerHoleCircle secondCircle)
      (setq upperHoleCenter firstCircleCenter)
      (setq lowerHoleCenter secondCircleCenter)
    )
    (progn
      (setq upperHoleCircle secondCircle)
      (setq lowerHoleCircle firstCircle)
      (setq upperHoleCenter secondCircleCenter)
      (setq lowerHoleCenter firstCircleCenter)
    )
  )

  ;; Nhập X tâm khóa mới
  (setq newLockCenterX (getreal "\nNhap X tam khoa moi: "))
  (setq offsetDistance (- newLockCenterX 45))

  (setq moveUpperHoleVector (list 0 (- (- offsetDistance 1)) 0))
  (setq moveLowerHoleVector (list 0 (- offsetDistance) 0))

  ;; Move 2 lỗ
  (command "_.move" upperHoleCircle "" '(0 0 0) moveUpperHoleVector)
  (command "_.move" lowerHoleCircle "" '(0 0 0) moveLowerHoleVector)

  ;; Cập nhật tâm mới
  (setq upperHoleCenter (cdr (assoc 10 (entget upperHoleCircle))))
  (setq lowerHoleCenter (cdr (assoc 10 (entget lowerHoleCircle))))


  ;; =================================================
  ;; PHẦN 2 – LẮP KHÓA MỚI
  ;; =================================================
  (prompt "\nChon khoa moi: ")
  (setq selectionSetNewLock
        (ssget '((0 . "CIRCLE,LINE,ARC,LWPOLYLINE,TEXT,MTEXT"))))
  (if (not selectionSetNewLock) (exit))

  ;; Lấy tâm khóa mới (1 circle duy nhất)
  (setq newLockCenter
        (cdr
          (assoc 10
            (entget
              (ssname (ssget "_P" '((0 . "CIRCLE"))) 0)
            )
          )
        )
  )

  ;; MOVE khóa mới theo lỗ trên
  (command "_.move" selectionSetNewLock "" "_none" newLockCenter "_none" upperHoleCenter)

  ;; COPY lỗ dưới (loại TEXT + RECTANGLE)
  (setq copySelectionSet (ssadd))
  (setq loopIndex 0)

  (repeat (sslength selectionSetNewLock)
    (setq currentEntity (ssname selectionSetNewLock loopIndex))
    (setq currentEntityData (entget currentEntity))
    (setq currentEntityType (cdr (assoc 0 currentEntityData)))

    (if (and
          (not (member currentEntityType '("TEXT" "MTEXT")))
          (not (and
                 (= currentEntityType "LWPOLYLINE")
                 (= (logand 1 (cdr (assoc 70 currentEntityData))) 1)
               ))
        )
      (ssadd currentEntity copySelectionSet)
    )

    (setq loopIndex (1+ loopIndex))
  )

  (command "_.copy" copySelectionSet "" "_none" upperHoleCenter "_none" lowerHoleCenter)

  (princ "\n🔥 TKKTHAYKHOA – CHAY ON DINH, BIEN AN TOAN.")
  (princ)
)
;;;;;;;;;;;;;;;;;;; End of TKK ;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; END of TKK ;;;;;;;;;;;;;;;;;;;