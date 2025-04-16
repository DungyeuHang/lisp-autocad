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

(defun c:APOINT (/ ent obj coords index ptList prefix count pt lastPt defLine allText ptText ptNext startPt startIndex closestDist i tmpPt finalPt finalNext dx dy dxStr dyStr)

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
      (setq startPt (getpoint "\n🧭 Chọn điểm đầu tiên để bắt đầu đánh số: "))
      (setq prefix (getstring T "\n📌 Nhập tiền tố điểm (vd: bl_fr): "))
      (setq count 1)
      (setq allText "")

      ;; ✅ đảo ptList để đi ngược chiều kim đồng hồ
      (setq ptList (reverse ptList))

      ;; ✅ tìm chỉ số gần nhất trong danh sách đã đảo
      (setq closestDist 1e99)
      (setq i 0)
      (foreach tmpPt ptList
        (if (< (distance tmpPt startPt) closestDist)
          (progn
            (setq closestDist (distance tmpPt startPt))
            (setq startIndex i)))
        (setq i (1+ i)))

      ;; ✅ xoay để điểm gần startPt nằm đầu danh sách
      (setq ptList (rotate-list ptList startIndex))

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
      (prompt "\n✅ Đã tạo xong danh sách APoint."))

    (prompt "\n❌ Đối tượng không phải Polyline!"))

  (princ))

  ;; Hỏi người dùng muốn đảo ngược danh sách điểm hay không (C: cùng chiều / N: ngược chiều kim đồng hồ)
  (initget "C N")
  (setq userInput (getkword "\nNhập chiều điểm (C: cùng chiều / N: ngược chiều kim đồng hồ): "))
  (if (equal userInput "C")
    (setq lst (reverse lst))
  )
