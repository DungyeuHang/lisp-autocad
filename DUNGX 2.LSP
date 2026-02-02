
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

  ;; =================================================
  ;; XÁC ĐỊNH TÂM KHÓA MỚI
  ;; - Nếu có CIRCLE → lấy tâm CIRCLE
  ;; - Nếu KHÔNG có CIRCLE → cho người dùng chọn điểm
  ;; =================================================
  (setq newLockCenter nil)
  (setq loopIndex 0)

  (repeat (sslength selectionSetNewLock)
    (setq currentEntity (ssname selectionSetNewLock loopIndex))
    (setq currentEntityData (entget currentEntity))
    (setq currentEntityType (cdr (assoc 0 currentEntityData)))

    (if (and (null newLockCenter) (= currentEntityType "CIRCLE"))
      (setq newLockCenter (cdr (assoc 10 currentEntityData)))
    )

    (setq loopIndex (1+ loopIndex))
  )

  ;; Nếu KHÔNG có CIRCLE → cho pick tâm
  (if (null newLockCenter)
    (progn
      (prompt "\n⚠️ Khóa mới KHÔNG có CIRCLE – vui lòng chọn tâm khóa mới.")
      (setq newLockCenter (getpoint "\nChon tam khoa moi: "))
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


(defun c:SAY_STRETCH_BY_DIM ( / dimNew dimOld valNew valOld delta d1 d2 doituong1)
  (prompt "\n=== STRETCH THEO DIM NGANG ===")

  ;; DIM CẦN THÀNH
  (setq dimNew (car (entsel "\nChọn DIM Doc cần thành: ")))
  (if (not dimNew) (exit))

  ;; DIM BAN ĐẦU
  (setq dimOld (car (entsel "\nChọn DIM Doc ban đầu: ")))
  (if (not dimOld) (exit))

  ;; LẤY GIÁ TRỊ
  (setq valNew (cdr (assoc 42 (entget dimNew))))
  (setq valOld (cdr (assoc 42 (entget dimOld))))

  (setq delta (- valNew valOld))

  (prompt (strcat "\nDelta = " (rtos delta 2 2)))

  ;; USER QUÉT VÙNG STRETCH
  (setvar "osmode" 16384)
  (setq d1 (list 0 0))
  (setq d2 (list 0  (+ 0 delta) ))
  (setq doituong1 (ssget))
  (command "stretch" doituong1 "" d1 d2  "") ;(command "stretch" )

  (prompt "\n✔ STRETCH Doc XONG.")
  (setvar "osmode" 16383)
  (princ)
)

(defun c:SAX_STRETCH_BY_DIM ( / dimNew dimOld valNew valOld delta d1 d2 doituong1 doituong2 d3)
  (prompt "\n=== STRETCH THEO DIM ngang ===")

  ;; DIM CẦN THÀNH
  (setq dimNew (car (entsel "\nChọn DIM ngang cần thành: ")))
  (if (not dimNew) (exit))

  ;; DIM BAN ĐẦU
  (setq dimOld (car (entsel "\nChọn DIM ngang ban đầu: ")))
  (if (not dimOld) (exit))

  ;; LẤY GIÁ TRỊ
  (setq valNew (cdr (assoc 42 (entget dimNew))))
  (setq valOld (cdr (assoc 42 (entget dimOld))))

  (setq delta (- valNew valOld))
  (setq delta (/ delta 2))
  (prompt (strcat "\nDelta = " (rtos delta 2 2)))

  ;; USER QUÉT VÙNG STRETCH
  (setvar "osmode" 16384)
  (setq d1 (list 0 0))
  (setq d2 (list (- 0 delta)  0 ))
  (setq doituong1 (ssget))
  (command "stretch" doituong1 "" d1 d2  "") ;(command "stretch" )
  (setq doituong2 (ssget))
  (setq d3 (list  (+ 0 delta) 0 ))
  (command "stretch" doituong2 "" d1 d3  "") ;(command "stretch" )
  
  (prompt "\n✔ STRETCH ngang XONG.")
  (setvar "osmode" 16384)
  (princ)
)

;; ================== HÀM GỘP SELECTION SET ==================
(defun ss-merge (ss1 ss2 / ss i)
  (setq ss (ssadd))
  (foreach s (list ss1 ss2)
    (if s
      (progn
        (setq i 0)
        (while (< i (sslength s))
          (ssadd (ssname s i) ss)
          (setq i (1+ i))
        )
      )
    )
  )
  ss
)
(defun mirrorByVerticalLine (ss / entLine data p1 p2 mid mirrorP1 mirrorP2)
  (prompt "\nChon LINE lam truc mirror (line dung): ")
  (setq entLine (car (entsel)))

  (if (and entLine
           (= "LINE" (cdr (assoc 0 (entget entLine)))))
    (progn
      (setq data (entget entLine))
      (setq p1 (cdr (assoc 10 data)))
      (setq p2 (cdr (assoc 11 data)))

      ;; tâm line
      (setq mid
        (list
          (/ (+ (car p1) (car p2)) 2.0)
          (/ (+ (cadr p1) (cadr p2)) 2.0)
          0.0
        )
      )

      ;; 2 điểm mirror (trục đứng)
      (setq mirrorP1 mid)
      (setq mirrorP2 (list (+ (car mid) 1.0) (cadr mid) 0.0))

      ;; mirror toàn bộ selection
      (command "_.MIRROR" ss "" mirrorP1 mirrorP2 "_N")
    )
    (prompt "\n❌ Phai chon LINE.")
  )
)

(defun ssnewer (ent / ss ent1)
	(if ent
		(progn
			(setq ent1 ent)
			(while (setq ent1 (entnext ent1))
				(if ent1
					(progn
						(if (NULL ss) (setq ss (ssadd)))
						(setq ss (ssadd ent1 ss))
					)
				)
			)
			ss
		)
		nil
	)	
)
;; ================== LỆNH CHÍNH ==================
(defun c:DCS
  (
    /
    oldos
    entLine dataLine p1 p2
    tam goc len n
    ssNew
    t0 t1 t2 t3 t4
    entPoly
    pStart pEnd entShort
    i pTo entNew
    d pLeft pIn
    ssBefore
    entBefore
    ssMirrorNew
    ssAll
  )


  ;; ===== LƯU & TẮT OSNAP =====
  (setq oldos (getvar "OSMODE"))
  (setvar "OSMODE" 0)

  ;; ===== TẠO / SET LAYER =====
  (if (not (tblsearch "LAYER" "_mss.bao"))
    (command "_.LAYER" "_M" "_mss.bao" "")
  )
  (setvar "CLAYER" "_mss.bao")

  ;; ===== CHỌN ĐOẠN THẲNG DÀI =====
  (setq entLine (entsel "\nChon doan thang dai: "))
  (if (and entLine
           (= "LINE" (cdr (assoc 0 (entget (car entLine))))))
    (progn
      (setq dataLine (entget (car entLine)))
      (setq p1 (cdr (assoc 10 dataLine)))
      (setq p2 (cdr (assoc 11 dataLine)))

      ;; ===== THÔNG SỐ =====
      (setq tam (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2))
      (setq goc (angle p1 p2))
      (setq len (distance p1 p2))
      (setq n   (fix (/ len 200.0)))

      (setq ssNew (ssadd))

      ;; ===== VẼ ĐA GIÁC MẪU =====
      (setq t0 tam)
      (setq t1 (polar t0 goc 1.25))
      (setq t2 (polar t1 (+ goc (/ pi 2)) 1.0))
      (setq t3 (polar t2 (+ goc pi) 2.5))
      (setq t4 (polar t3 (+ goc (* 3 (/ pi 2))) 1.0))

      (command "_.PLINE" t1 t2 t3 t4 "")
      (setq entPoly (entlast))

      (command "_.FILLET" "_R" 1)
      (command "_.FILLET" "_P" entPoly)

      (ssadd entPoly ssNew)

      ;; ===== ĐOẠN NGẮN 97.5 =====
      (setq pStart (polar tam (+ goc pi) 1.25))
      (setq pEnd   (polar pStart (+ goc pi) 97.5))
      (command "_.LINE" pStart pEnd "")
      (setq entShort (entlast))
      (ssadd entShort ssNew)

      ;; ===== COPY THEO BƯỚC 100 =====
      (setq i 1)
      (repeat n
        (setq pTo (polar t0 (+ goc pi) (* i 100.0)))

        (command "_.COPY" entPoly "" t0 pTo)
        (setq entNew (entlast))
        (ssadd entNew ssNew)

        (if (< i n)
          (progn
            (command "_.COPY" entShort "" t0 pTo)
            (setq entNew (entlast))
            (ssadd entNew ssNew)
          )
        )
        (setq i (1+ i))
      )

      ;; ===== ĐOẠN KÉO =====
      (setq d (/ (- len (* 100.0 2 n)) 2.0))
      (setq d (- d 1.25))
      (setq pLeft (polar tam (+ goc pi) (/ len 2.0)))
      (setq pIn   (polar pLeft goc d))
      (command "_.LINE" pLeft pIn "")
      (ssadd (entlast) ssNew)

      ;; ===== XOÁ ĐOẠN GỐC =====
      (entdel (car entLine))

      ;; ===== MIRROR =====
      (setq ssBefore ssNew)
      (setq entBefore (entlast))

      (command
        "_.MIRROR"
        ssNew
        ""
        tam
        (polar tam (+ goc (/ pi 2)) 10)
        "_N"
      )
      (setq ssMirrorNew (ssnewer entBefore))
      (setq ssAll (ss-merge ssNew ssMirrorNew))

      ;; xoá đa giác mẫu
      (if (and entPoly (entget entPoly))
        (entdel entPoly)
      )
      (if (and ssAll (> (sslength ssAll) 0))
        (mirrorByVerticalLine ssAll)
      )


    )
  )

  ;; ===== ĐỔI LAYER ĐỒNG LOẠT =====
  (command "_.CHPROP" ssAll "" "_LA" "_mss.bao" "")

  ;; ===== TRẢ LẠI OSNAP =====
  (setvar "OSMODE" oldos)
  (princ)
)


