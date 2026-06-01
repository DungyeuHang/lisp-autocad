
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
        (setvar "osmode" 16384)

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
      (setvar "osmode" 16383)

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
      (setq textInsertPt (list (+ (car finalPt) 5) (- (cadr finalPt) 0)))
      (setq arcInfoStr "")
      (setq count 0)
      (while (< count (- (length ptList) 1))
        (setq p1 (nth count ptList))
        (setq p2 (nth (1+ count) ptList))
        (setq bulge (vla-GetBulge obj count))
        (if (> (abs bulge) 1e-6)
          (progn
            (setq chord (distance (list (car p1) (cadr p1)) (list (car p2) (cadr p2))))
            (setq theta (* 4 (atan bulge))) ; góc ở tâm (radian)
            (setq radius (/ chord (* 2 (sin (/ theta 2))))) ; bán kính R = chord / (2 * sin(theta/2))
            ; công thức R = chord / (4 * bulge)
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


;; <<<<<<<<<<<<<<<<<<<        End of  Make Points by Polyline      >>>>>>>>>>>>>>>>>



;START ............................... UN-FILLET ..................................... =========>;
(vl-load-com)

(defun c:UFF ( / ent obj n i pts p0 p1 p2 p3 ang1 ang2 ipt)
  (setvar "clayer" "_mss.phantom")
  (setq ent (car (entsel "\nChọn polyline cần bỏ fillet (giữ 1 điểm giao, bỏ cả đầu và cuối cung): ")))
  (if ent
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (if (= (vla-get-ObjectName obj) "AcDbPolyline")
        (progn
          (setq n (1+ (fix (vlax-curve-getEndParam ent))))
          (setq pts '())

          (setq i 0)
          (while (< i n)
            (setq bulge (vla-GetBulge obj i))
            (if (= bulge 0.0)
              ;; không phải cung -> giữ nguyên điểm
              (setq pts (append pts (list (vlax-curve-getPointAtParam ent i))))
              ;; là cung -> chỉ lấy giao, bỏ điểm đầu và cuối
              (progn
                (setq p0 (vlax-curve-getPointAtParam ent i))
                (setq p3 (vlax-curve-getPointAtParam ent (1+ i)))
                (setq p1 (vlax-curve-getPointAtParam ent (max 0 (- i 1))))
                (setq p2 (if (< (+ i 2) n)
                           (vlax-curve-getPointAtParam ent (+ i 2))
                           p3))
                ;; tìm giao hai đường thẳng
                (setq ang1 (angle p1 p0))
                (setq ang2 (angle p3 p2))
                (setq ipt (inters p0 (polar p0 ang1 1.0)
                                  p3 (polar p3 ang2 1.0) nil))
                ;; thêm đúng 1 điểm giao
                (setq pts (append pts (list ipt)))
                ;; nhảy qua luôn điểm kế tiếp (p3) để bỏ nó
                (setq i (1+ i))
              )
            )
            (setq i (1+ i))
          )

          ;; tạo polyline mới
          (setq ms (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
          (setq arr (vlax-make-safearray vlax-vbDouble (cons 0 (1- (* 2 (length pts))))))
          (setq k 0)
          (foreach p pts
            (vlax-safearray-put-element arr (* k 2) (car p))
            (vlax-safearray-put-element arr (1+ (* k 2)) (cadr p))
            (setq k (1+ k))
          )
          (vla-AddLightWeightPolyline ms arr)
          (princ "\n>>> Đã bỏ fillet và chỉ giữ 1 điểm giao, bỏ cả đầu & cuối cung! <<<")
        )
        (princ "\nĐối tượng không phải polyline!")
      )
    )
  )
  (princ)
)

;<=============.............................. UN-FILLET ......................................... END;
;***************************************************************************************************************************************************************************************************************


(defun cmmm-line-p (ent / typ)
  (and ent
       (setq typ (cdr (assoc 0 (entget ent))))
       (member typ '("LINE" "LWPOLYLINE" "POLYLINE")))
)

(defun cmmm-select-line (msg allowExit allowCustomBase / pick ent ok)
  (setq ok nil)
  (while (not ok)
    (if allowCustomBase
      (initget "C")
    )
    (setq pick (entsel msg))
    (cond
      ((and allowCustomBase
            (= (type pick) 'STR)
            (= (strcase pick) "C"))
       (setq ent 'CMMM-CUSTOM-BASE)
       (setq ok T)
      )
      ((null pick)
       (if allowExit
         (setq ok 'exit)
         (prompt "\nBan chua chon doi tuong. Vui long chon lai LINE/PLINE.")
       )
      )
      ((not (cmmm-line-p (car pick)))
       (prompt "\nDoi tuong vua chon khong phai LINE/PLINE. Vui long chon lai.")
      )
      (T
       (setq ent (car pick))
       (setq ok T)
      )
    )
  )
  (if (= ok 'exit) nil ent)
)

(defun cmmm-get-selection-center (ss / idx ent obj minVar maxVar curMin curMax allMin allMax)
  (setq idx 0)
  (while (< idx (sslength ss))
    (setq ent (ssname ss idx))
    (if (/= "DIMENSION" (cdr (assoc 0 (entget ent))))
      (progn
        (setq obj (vlax-ename->vla-object ent))
        (if (vlax-method-applicable-p obj 'GetBoundingBox)
          (progn
            (vla-getBoundingBox obj 'minVar 'maxVar)
            (setq curMin (vlax-safearray->list minVar))
            (setq curMax (vlax-safearray->list maxVar))
            (if allMin
              (progn
                (setq allMin
                  (list
                    (min (car allMin) (car curMin))
                    (min (cadr allMin) (cadr curMin))
                    (min (caddr allMin) (caddr curMin))
                  )
                )
                (setq allMax
                  (list
                    (max (car allMax) (car curMax))
                    (max (cadr allMax) (cadr curMax))
                    (max (caddr allMax) (caddr curMax))
                  )
                )
              )
              (progn
                (setq allMin curMin)
                (setq allMax curMax)
              )
            )
          )
        )
      )
    )
    (setq idx (1+ idx))
  )
  (if allMin
    (mapcar '(lambda (a b) (/ (+ a b) 2.0)) allMin allMax)
    nil
  )
)

(defun c:CMMM (/ *error* oldos ss basePt newBasePt done line1 line2 p1a p1b p2a p2b mid1 mid2 destPt vlaLine1 vlaLine2)
  (vl-load-com)

  (defun *error* (msg)
    (if oldos
      (setvar "OSMODE" oldos)
    )
    (if (and msg
             (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
      (prompt (strcat "\nLoi: " msg))
    )
    (princ)
  )

  (setq oldos (getvar "OSMODE"))

  (prompt "\nChon cac doi tuong can copy: ")
  (setq ss (ssget))

  (if ss
    (progn
      (setvar "OSMODE" 0)
      (setq basePt (cmmm-get-selection-center ss))
      (if basePt
        (prompt "\nMac dinh dung tam nhom lam diem goc. Neu muon doi, go C tai buoc chon LINE/PLINE thu nhat.")
        (progn
          (prompt "\nKhong tinh duoc tam nhom. Vui long chon diem goc.")
          (setq basePt (getpoint "\nChon diem goc de copy: "))
        )
      )

      (if basePt
        (progn
          (setq done nil)

          (while (not done)
            (prompt "\n--- Chon cap LINE moi (Enter de thoat) ---")
            (setq line1 (cmmm-select-line "\nChon LINE/PLINE thu nhat hoac [C] de doi diem goc: " T T))

            (if (not line1)
              (setq done T)
              (progn
                (if (= line1 'CMMM-CUSTOM-BASE)
                  (progn
                    (setq newBasePt (getpoint "\nChon diem goc de copy: "))
                    (if newBasePt
                      (setq basePt newBasePt)
                      (prompt "\nKhong chon diem goc. Giu nguyen diem goc hien tai.")
                    )
                  )
                  (progn
                    (setq line2 (cmmm-select-line "\nChon LINE/PLINE thu hai: " nil nil))

                    (setq vlaLine1 (vlax-ename->vla-object line1))
                    (setq vlaLine2 (vlax-ename->vla-object line2))

                    (setq p1a (vlax-curve-getStartPoint vlaLine1))
                    (setq p1b (vlax-curve-getEndPoint vlaLine1))
                    (setq mid1 (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1a p1b))

                    (setq p2a (vlax-curve-getStartPoint vlaLine2))
                    (setq p2b (vlax-curve-getEndPoint vlaLine2))
                    (setq mid2 (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p2a p2b))

                    (setq destPt (mapcar '(lambda (a b) (/ (+ a b) 2.0)) mid1 mid2))

                    (command "_COPY" ss "" basePt destPt)
                  )
                )
              )
            )
          )
        )
      )
    )
    (prompt "\nKhong co doi tuong nao duoc chon.")
  )
  (setvar "OSMODE" oldos)
  (princ)
)


;;;;;;;;;;;;;;;;;;;;;;;;; 

;;;;;; Start VVD ;;;;;;
(defun safeFormatNumber (n)
  "Nếu n là số nguyên, trả về chuỗi số nguyên; nếu không, trả về chuỗi số với 4 chữ số sau dấu thập phân."
  (if (equal n (fix n) 1e-6)
      (itoa (fix n))
      (rtos n 2 4)
  )
)

(defun formatOffset (n)
  "Trả về chuỗi offset với dấu và số được định dạng.
Nếu n = 0 (trong khoảng 1e-6), trả về chuỗi rỗng.
Nếu n > 0, trả về chuỗi \" + <n>\";
nếu n < 0, trả về chuỗi \" - <abs(n)>\"."
  (if (equal n 0 1e-6)
      ""
      (if (> n 0)
          (strcat " + " (safeFormatNumber n))
          (strcat " - " (safeFormatNumber (- n)))
      )
  )
)

(defun c:vvd (/ prefix pt counter allText currentDef dxVal dyVal dxStr dyStr lastPt finalPt ptText secondPt)
  (prompt "\n🚀 Đang xử lý...")
  (setvar "clayer" "_mss.phantom")
  (setvar "osmode" 5)
  ;; 1. Nhập tiền tố (ví dụ: "od_on_ol")
  (setq prefix (getstring "\nNhập tiền tố: "))
  (setq counter 1)
  (setq allText "")  ; chuỗi tích lũy toàn bộ định nghĩa

  ;; 2. Nhập các điểm
  (while (setq pt (getpoint (strcat "\nChọn điểm " (itoa counter) " (ESC/Enter để dừng): ")))
    ;; Vẽ hình tròn bán kính 1 tại điểm đã chọn
    (command "CIRCLE" pt "0.3")
    
    ;; Xác định vị trí đặt MTEXT cho định nghĩa của điểm:
    ;; Nếu counter lẻ: đặt text bên trên circle (y + 1),
    ;; Nếu counter chẵn: đặt text bên dưới circle (y - 1).
    (setq ptText (if (= (rem counter 2) 1)
                 (list (car pt) (+ (cadr pt) 0.1) (if (numberp (caddr pt)) (caddr pt) 0))
                 (list (car pt) (- (cadr pt) 0.1) (if (numberp (caddr pt)) (caddr pt) 0))
          ))

    
    (if (= counter 1)
      ;; Điểm đầu tiên: vd: od_on_ol_p1 = APoint(od_on_ol_p1.x, od_on_ol_p1.y)
      (setq currentDef (strcat prefix "_p" (itoa counter)
                                " = APoint(" 
                                prefix "_p" (itoa counter) ".x, " 
                                prefix "_p" (itoa counter) ".y)"))
      ;; Các điểm sau: tính độ lệch so với điểm trước đó
      (progn
        (setq dxVal (- (car pt) (car lastPt)))
        (setq dyVal (- (cadr pt) (cadr lastPt)))
        (setq dxStr (formatOffset dxVal))
        (setq dyStr (formatOffset dyVal))
        (setq currentDef (strcat prefix "_p" (itoa counter)
                                " = APoint(" 
                                prefix "_p" (itoa (1- counter)) ".x" dxStr ", " 
                                prefix "_p" (itoa (1- counter)) ".y" dyStr ")"))
      )
    )
    
    ;; Xác định hộp MTEXT dựa theo vị trí ptText:
    ;; Nếu counter lẻ, đặt hộp bên trên (x + 190, y + 2);
    ;; nếu counter chẵn, đặt hộp bên dưới (x + 190, y - 2).
    (setq secondPt (if (= (rem counter 2) 1)
                   (list (+ (car ptText) 10) (+ (cadr ptText) 0.1))
                   (list (+ (car ptText) 10) (- (cadr ptText) 0.1))
            ))

    (command "MTEXT" ptText secondPt currentDef "")
    
    ;; Tích lũy định nghĩa vào chuỗi allText, mỗi định nghĩa xuống dòng bằng "\n"
    (setq allText (strcat allText currentDef "\n"))
    
    (setq lastPt pt)
    (setq counter (1+ counter))
  )
  
  ;; 3. Sau khi nhập điểm, nếu có định nghĩa thì yêu cầu chọn vị trí để đặt MTEXT tổng hợp
  (if (> (strlen allText) 0)
    (progn
      (setq finalPt (getpoint "\nChọn vị trí để đặt text tổng hợp: "))
      (setq secondPt (list (+ (car finalPt) 10) (+ (cadr finalPt) 0.1)))
      (command "MTEXT" finalPt secondPt allText "")
    )
  )
  (setvar "osmode" 16383)
  (prompt "\n✅ ĐÃ XỬ LÝ XONG.")
  (princ)
)

(princ "\nGõ VE_DIEM để chạy chương trình.\n")
(princ)

;;;;;; End VVD ;;;;;;

