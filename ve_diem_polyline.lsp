
;; <<<<<<<<<<<<<<<<<<<          Make Points by Polyline      >>>>>>>>>>>>>>>>>
(defun safeFormatNumber (n)
  (if (equal n (fix n) 1e-6)
    (itoa (fix n))
    (rtos n 2 4)))

(defun formatOffset (n)
  (cond
    ((equal n 0 1e-6) "")
    ((> n 0) (strcat " + " (safeFormatNumber n)))
    ((< n 0) (strcat " - " (safeFormatNumber (- n))))))

(defun sublist-from (lst n)
  (if (<= n 0)
    lst
    (sublist-from (cdr lst) (1- n))))

(defun take-n (lst n)
  (if (or (null lst) (= n 0))
    '()
    (cons (car lst) (take-n (cdr lst) (1- n)))))

(defun rotate-list (lst n)
  (append (sublist-from lst n) (take-n lst n)))

(defun c:APOINT (/ ent obj coords index ptList prefix count pt lastPt defLine allText ptText ptNext startPt startIndex closestDist tmpPt finalPt finalNext dx dy dxStr dyStr)
  (setvar "clayer" "_mss.phantom")
  (setq ent (car (entsel "\n🎯 Chọn polyline: ")))
  (if (and ent (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE"))
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (setq coords (vlax-get obj 'Coordinates))
      (setq ptList '())
      (setq index 0)
      (while (< index (length coords))
        (setq pt (list (nth index coords) (nth (+ index 1) coords)))
        (setq ptList (append ptList (list pt)))
        (setq index (+ index 2)))

      ;; chọn điểm bắt đầu
      ;(setq startPt (getpoint "\n🧭 Chọn điểm đầu tiên để bắt đầu đánh số: "))
      (setq prefix (getstring T "\n📌 Nhập tiền tố điểm (vd: bl_fr): "))
      (setq count 1)
      (setq allText "")


      ;; ✅ vẽ và đánh số từng điểm
      (foreach pt ptList
        (command "CIRCLE" pt "0.3")

        (setq ptText (if (= (rem count 2) 1)
                       (list (car pt) (+ (cadr pt) 0.1))
                       (list (car pt) (- (cadr pt) 0.1))))

        (if (= count 1)
          (setq defLine (strcat prefix "_p" (itoa count)
                                " = APoint("
                                (safeFormatNumber (car pt)) ", "
                                (safeFormatNumber (cadr pt)) ")"))
          (progn
            (setq dx (- (car pt) (car lastPt)))
            (setq dy (- (cadr pt) (cadr lastPt)))
            (setq dxStr (formatOffset dx))
            (setq dyStr (formatOffset dy))
            (setq defLine (strcat prefix "_p" (itoa count)
                                  " = APoint(" prefix "_p" (itoa (1- count)) ".x" dxStr ", "
                                                 prefix "_p" (itoa (1- count)) ".y" dyStr ")"))))

        (setq ptNext (if (= (rem count 2) 1)
                       (list (+ (car ptText) 10) (+ (cadr ptText) 0.1))
                       (list (+ (car ptText) 10) (- (cadr ptText) 0.1))))
        (command "MTEXT" ptText ptNext defLine "")

        (setq allText (strcat allText defLine "\n"))
        (setq lastPt pt)
        (setq count (1+ count)))

      ;; ✅ chèn tổng hợp định nghĩa nếu có
      (if (> (strlen allText) 0)
        (progn
          (setq finalPt (getpoint "\n📄 Chọn điểm đặt toàn bộ định nghĩa text: "))
          (setq finalNext (list (+ (car finalPt) 10) (+ (cadr finalPt) 1)))
          (command "MTEXT" finalPt finalNext allText "")))
      (prompt "\n✅ Đã tạo xong danh sách APoint.")

      ;; ========================
      ;; ✅ Thêm Smart Shape Text
      ;; ========================
      (setq textInsertPt (list (car finalPt) (- (cadr finalPt) 1)))
      (setq arcInfoStr "")
      (setq count 0)
      (while (< count (- (length ptList) 1))
        (setq p1 (nth count ptList))
        (setq p2 (nth (1+ count) ptList))
        (setq bulge (vla-GetBulge obj count))
        (if (> (abs bulge) 1e-6)
          (progn
            (setq chord (distance (list (car p1) (cadr p1)) (list (car p2) (cadr p2))))
            (setq radius (/ chord (* 4 bulge))) ; công thức R = chord / (4 * bulge)
            (setq arcInfoStr
              (strcat arcInfoStr
                (strcat "(" (itoa (1+ count)) "," (itoa (+ 2 count)) "): (" (rtos radius 2 2) ", True),\n")
              )
            )
          )
        )
        (setq count (1+ count))
      )


      (if (= (vla-get-Closed obj) :vlax-true)
        (setq closeStr "True")
        (setq closeStr "False")
      )

      (setq smartShapeStr
        (strcat "create_smart_shape(\""
                prefix "_p\", 1, "
                (itoa (length ptList))
                ", arcs_info={\n"
                arcInfoStr
                "},close ="
                closeStr
                ")")
      )

      ;; Chèn đoạn text vào bản vẽ
      (entmakex
        (list
          (cons 0 "TEXT")
          (cons 10 textInsertPt)
          (cons 40 0.1)
          (cons 1 smartShapeStr)
        )
      )
)

    (prompt "\n❌ Đối tượng không phải Polyline!"))

  (princ))