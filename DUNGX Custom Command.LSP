;Lệnh line = 1
(defun c:1 ()
  (setvar "osmode" 16383)
  ;(command "osmode" 16383)
  (setvar "clayer" "_mss.bao")
  ;(command "_.LAYER" "_SET" "_mss.bao" "")
  (command "line"))

;Lệnh move  = 2
(defun c:2 ()
;(command "osmode" 16383)
  (setvar "osmode" 16383)
(command "move"))

;Lệnh insert = 3 không dùng được lisp


;Lệnh layon = 4
(defun c:4 ()
    (command "layon"))

;Lệnh layoff = 5
(defun c:5 ()
    (command "layoff"))

;lệnh mocoro -11
(defun c:11 ()
  ;(command "osmode" 16383)
  (setvar "osmode" 16383)
(command "mocoro"))

;lệnh mirror -33
(defun c:33 ()
  ;(command "osmode" 16383)
  (setvar "osmode" 16383)
  (command "mirror"))

;lệnh matchprop -44
(defun c:44 ()
    (command "matchprop"))

;lệnh copy -cc không lặp lại nhiều lần


;lệnh quickselect- qq
(defun c:qq ()
    (command "qselect"))

;lệnh filter - ff
(defun c:ff ()
    (command "filter"))

;lệnh dimlinear -dd
(defun c:dd ()
  (setvar "osmode" 16383)
  ;(command "_.LAYER" "_SET" "_mss.kichthuoc" "")
  (setvar "clayer" "_mss.kichthuoc")
  (command "dimlinear"))

;lệnh find-f1
(defun c:f1 ()
    (command "find"))

;lệnh dimspace -dss
(defun c:dss ()
    (command "dimspace"))

;lệnh dimcontinue -dcc
(defun c:dcc ()
    (command "dimcontinue"))

;Center mid mode c1
(defun c:c1()
  ;(command "osmode" "1030")
  (setvar "osmode" 1030)
  (princ))
;end of c1


;             < Copy and rotate -CR >        ;
( defun c:cr(/ diemdau diemden dt)
  (setq dt (ssget))
  (setq diemdau (getpoint "chon diem dau" )) ;chon diem dau
  (setq diemden (getpoint "chon diem den" )) ;chon diem den
  (command "mocoro" dt "" diemdau "copy" diemden "" "rotate" "" diemden)
 )
;;;;;;;;;; end of CR

;       <<< Copy to mid and erase cs -CES >>>         ;
(defun c:CES (/ dt diemdau ktc kt d1 d2 )
  ;(command "osmode" 16384)
  (setvar "osmode" 16384)
  (setq diemdau (list 0 0))
  (setq ktc (getreal "\nnhap kich thuoc canh chinh"))
  (setq kt (+ ktc 5.5))
  (setq dt (ssget))
  (setq d1 (list (+ (car diemdau) kt)(cadr diemdau)))
  (setq d2 (list (- (car diemdau) kt)(cadr diemdau)))
  (command "copy" dt ""  diemdau d1 "")
  (command "copy" dt ""  diemdau d2 "")
 (command "erase" dt "")
  
  ;(command "osmode" 16383)
  (setvar "osmode" 16383)
  (princ)
)
;;;;;;;;;; end of CES ;;;;;;;;;



;;;;;;;;;;;;;;;;;;             < Dungx insert -dungx >            ;;;;;;;;;;;;;;
(defun c:dungx (/ pt time date-string year month day formatted-date new-pt new-pt1 i)
  ;(command "_.LAYER" "_SET" "_mss.phantom" "")
  (setvar "clayer" "_mss.phantom")
  (setq i 1) ;; Bắt đầu đếm từ 1
  ;(command "osmode" "16384")
  (setvar "osmode" 16384)

  ;; Vòng lặp vô hạn, chỉ dừng khi người dùng nhấn Enter hoặc Space
  (while (setq pt (getpoint "\nChọn điểm chèn (Nhấn Enter để thoát): "))
    
    ;; Kiểm tra nếu block "SIGNATURE" không tồn tại
    (if (not (tblsearch "block" "SIGNATURE"))
      (progn
        (princ "\nBlock SIGNATURE không tồn tại. Hãy kiểm tra lại.")
        (exit)
      )
    )
    
    ;; Chèn block SIGNATURE tại điểm chèn
    (command "insert" "SIGNATURE" pt "" "")

    ;; Lấy giá trị thời gian từ AutoCAD
    (setq time (getvar "Cdate"))

    ;; Xử lý chuỗi ngày tháng từ giá trị thời gian
    (setq date-string (rtos time 2 6)) ;; Chuyển về chuỗi có 6 số lẻ
    (setq date-string (substr date-string 1 8)) ;; Lấy 8 ký tự đầu tiên

    ;; Tách năm, tháng, ngày
    (setq year (substr date-string 1 4))     ;; Năm: yyyy
    (setq month (substr date-string 5 2))    ;; Tháng: MM
    (setq day (substr date-string 7 2))      ;; Ngày: dd

    ;; Tạo chuỗi văn bản theo định dạng Ngày: DD/MM/YYYY
    (setq formatted-date (strcat day "/" month "/" year))

    ;; Tạo điểm mới để hiển thị ngày và số thứ tự
    (setq new-pt (list (-(car pt) 200) (+ (cadr pt) -150)))
    (setq new-pt1 (list (-(car pt) 100) (+ (cadr pt) -250)))

    ;; Hiển thị chuỗi văn bản
    (command "text" new-pt 55 "0" formatted-date)
    (command "text" new-pt1 55 "0" (strcat "NO." (itoa i))) ;; Đếm số thứ tự và chuyển thành chuỗi

    ;; Tăng giá trị đếm
    (setq i (1+ i))
  )

  ;; Khi thoát vòng lặp, chạy các lệnh cuối
  ;(command "osmode" "16383")
  (setvar "osmode" 16383)
  ;(command "_.LAYER" "_SET" "_mss.kichthuoc" "")
  (setvar "clayer" "_mss.kichthuoc")
  (princ)
)


;;;;;;;;;;;;;;;;;;;;  end of DUNGX  ;;;;;;;;;;;;;



;       <<<<<<<< full osmode -SS >>>>>>>          ;
( defun c:ss()
 ;(command "osmode" "16383")
  (setvar "osmode" 16383)
  (princ)
 )
;;;;;;;;;;; end of SS ;;;;;;;



;         <<<<<<<<<<<< Keo Canh Lech danh cho cua 4 canh - KCL >>>>>>>>>> ;
( defun c:KCL(/ d1 diemtam ktk doituong1 doituong2 doituong3)
  ;(command "osmode" 16384)
  (setvar "osmode" 16384)
  (setq d1 (list 0 0))
  (setq ktk (getreal "\nhap kich thuoc keo"))
  (setq doituong1 (ssget))
  (command "stretch" doituong1 "" d1 (list (- (car d1) ktk) (cadr d1) ) "")
  (setq doituong2 (ssget))
  (command "stretch" doituong2 "" d1 (list (+ (car d1) ktk) (cadr d1) ) "")
  (setq doituong3 (ssget))
  (command "stretch" doituong3 "" d1 (list (car d1) (+ (cadr d1) ktk ) ) "")
  ;(command "osmode" 16383) 
  (setvar "osmode" 16383)  
  (princ)
 )
;;;;;;;;;;;; end of KCL ;;;;;;;;;;




;     <<<<<<<<<< Keo cua so 3 canh -KCS3 >>>>>>>>>>>> ;
(defun c:kcs3 (/ d1 kto dk kt1 kt4 ktk doituongx doituongm doituong1 doituong2 doituong3)
  ;(command "osmode" "16384")
  (setvar "osmode" 16384)
  (setq d1 (list 0 0))
  (setq kto (getreal "\nNhập kích thước ô chờ: "))
  (initget 0)
  (setq dk (getreal "\nNhập kích thước dày khuôn <42.5>: "))
  (if (null dk) (setq dk 42.5))
  (setq kt1 (/ (- kto (* 2 (- dk 12.5) ) 17 ) 3) )
  (setq kt4 (/ ( - kto (* 2 (- dk 12.5 )  ) 20 ) 4))
  (setq ktk (- kt1 kt4) )
  (setq doituongx (ssget))
  (command "erase" doituongx "")
  (setq doituongm (ssget))
  (command "move" doituongm "" d1 (list (- (car d1) ktk) (cadr d1) ) "") ;move canh chinh qua trai
  (setq doituong1 (ssget))
  (command "stretch" doituong1 "" d1 (list (- (car d1) ktk) (cadr d1) ) "") ;keo trai
  (setq doituong2 (ssget))
  (command "stretch" doituong2 "" d1 (list (+ (car d1) ktk) (cadr d1) ) "") ;keo phai 
  (setq doituong3 (ssget))
  (command "stretch" doituong3 "" d1 (list (car d1) (+ (cadr d1) ktk ) ) "") ;keophoi
  (setvar "osmode" 16383)
  ;(command "osmode" "16383")   
  (princ)
  )
;;;;;;;;; end of KCS3 ;;;;;;;;




;          <<<<<<<< LayOff 5 layer de lam phoi -OOO >>>>>>>> ;
(defun c:ooo()
       (command "layoff" "" "layer" "off" "_mss.dut" "" "Y" "" )
       (command "layoff" "" "layer" "off" "_mss.cat" "" "Y" "" )
  	(command "layoff" "" "layer" "off" "_mss.phantom" "" "Y" "" )
  	(command "layoff" "" "layer" "off" "_mss.pro" "" "Y" "" )
	(command "layoff" "" "layer" "off" "_mss.canh" "" "Y" "" )
       (command "layoff" "" "layer" "off" "DUNGX" "" "Y" "" )
(princ)
  )
;;;;;;;;;;; end of OOO ;;;;;;;;;

;         <<<<< set last -LL >>>>
(defun c:ll ()
  (setq l (getreal "nhap so can set cho L"))
  (setq k (/ l 2))
 )
;;;;;;;; end of LL ;;;;;;




;        <<<<<<<< Stretch X -SX >>>>>> ;
(defun c:sx (/ a b keyword hs d1 doituong1 doituong2)
  (setq a (getreal "nhap so can set cho a"))
  (setq b (/ a 2))
  (initget 1 "t g") ;tron thoi vuong
  (setq keyword (getkword "Ban muon stretch TANG <t> hay GIAM <g>"))
  (cond
    ((= keyword "t") ;he so 
     (setq hs 1)) ;
    ((= keyword "g") ;
     (setq hs -1 )) ; 
     ) ;ket thuc cond
  (setq d1 (list 0 0))
  ;(command "osmode" "16384")
  (setvar "osmode" 16384)
 (while
(setq doituong1 (ssget))
  (setq doituong2 (ssget))
  (command "stretch" doituong1 "" d1 (list (- (car d1) (* hs b)) (cadr d1) ) "") ;(command "stretch" )
  (command "stretch" doituong2 "" d1 (list (+ (car d1) (* hs b)) (cadr d1) ) "")
)
  ;(command "osmode" "16383")
  (setvar "osmode" 16383)
)
;;;;;   end of SX   ;;;;;




;         <<<<<< Stretch Y -SY >>>>>>> ;
(defun c:SY (/ a b keyword hs d1 doituong1 doituong2)
  (setq a (getreal "nhap so can set cho a"))
  (setq b (/ a 2))
  (initget 1 "t g") ;
  (setq keyword (getkword "Ban muon stretch TANG <t> hay GIAM <g>"))
  (cond
    ((= keyword "t") ;he so 
     (setq hs 1)) ;
    ((= keyword "g") ;
     (setq hs -1 )) ; 
     ) ;ket thuc cond
  ;(command "osmode" "16384")
  (setvar "osmode" 16384)
  (setq d1 (list 0 0))
(while
  (setq doituong1 (ssget))
  (setq doituong2 (ssget))
  (command "stretch" doituong1 "" d1 (list (car d1) (+ (cadr d1) (* hs b)) ) "") ;(command "stretch" )
  (command "stretch" doituong2 "" d1 (list (car d1) (- (cadr d1) (* hs b)) ) "")
)
  ;(command "osmode" "16383")
  (setvar "osmode" 16383)
  )
;;;;;;;;;;; end of SY ;;;;;;;;;;; 

;         <<<<<<<  Ve lo song -VLS >>>>>>
(defun c:vls(/ dd dt diemtam doituong sosong1 kctam)
  ;(command "osmode" "16384")
  (setvar "osmode" 16384)
  (setq dd (getreal "\nnhap chieu dai do"))
  (setq dt (+ (/ dd 2) 70)) ;chieu dai tinh
  (setq diemtam (list 0 0))
  (setq doituong (ssget))
  (setq sosong1 (+ (fix (/ dt 100 ) ) 1) )
  (setq kctam (/ dt sosong1))
  (command "copy" doituong "" diemtam "_array" sosong1 (list (+ (car diemtam) kctam) (cadr diemtam) ) )
  (command "copy" doituong "" diemtam "_array" sosong1 (list (- (car diemtam) kctam) (cadr diemtam) ) )
  ;(command "osmode" "16383")
  (setvar "osmode" 16383)
  (princ)
)
;;;;;;;;;;;; end of VLS ;;;;;;;;

;       <<<<<<<<<<< VE SONG 3 KIEU -VSS >>>>>>>>> ;
(defun c:vss (/ dd dk keyword diemtam doituong sls_1 sls_test tam_lo_song kctam sl sosong1 diemdim1 diemdim2 diemdim3 diemdim4 diemdim5 diemdim6 diemdim7 diemdim8 diemtext)
;(command "osmode" "16383")
(setvar "osmode" 16383)
(initget 1 "tr th v") ;tron thoi vuong
  (setq keyword (getkword "Ban muon ve song tron <tr>, thoi <th> hay song vuong <v>"))
  (cond
    ((= keyword "tr") ;tron r=17.25
     (setq dk 17.25)) ; ket thuc dk1
    ((= keyword "th") ;thoi r= 20.75
     (setq dk 20.75 )) ; ket thuc dk2
    ((= keyword "v") ;vuong  = 15.5
     (setq dk 15.5 )) ; ket thuc dk3
     ) ;ket thuc cond
  (setq dd (getreal "\nnhap chieu dai do"))
  
  ;(setq dt (+ (/ dd 2) dk)) ;chieu dai tinh
  (setq diemtam (getpoint "chon diem tam"))
  (setq doituong (ssget))
  ;(setq sosong1 (+ (fix (/ dt 155 ) ) 1) )
  ;(setq kctam (/ dt sosong1))
  ;(setq sl  (- (* sosong1 2) 1))
  ;(setq sls_test (list 21 19 17 15 13 11 9 7 5 3 1 ))
  (foreach sls_1 (list 21 19 17 15 13 11 9 7 5 3 )
          (setq sls_test (- sls_1 2))
          ;(setq khoang_sang (/ (- dd (* sls_test dk)) (+ sls_test 1)))
          (setq tam_lo_song (/ (+ dd (* 2 dk)) (+ sls_test 1)))

    
(if (and (> tam_lo_song 130) (< tam_lo_song 160)) ;if
     (and (setq kctam tam_lo_song)
       (setq sl sls_test)) ;then
        (if (and (> tam_lo_song 100) (< tam_lo_song 130))
          (and (setq kctam tam_lo_song)
          (setq sl sls_test))
        ) ;else
      )
  )

  (setq sosong1 (+ (fix (/ sl 2)) 1))
  
  ;(command "osmode" "16384")
  (setvar "osmode" 16384)
  (command "copy" doituong "" diemtam "_array" sosong1 (list (+ (car diemtam) kctam) (cadr diemtam) ) )
  (command "copy" doituong "" diemtam "_array" sosong1 (list (- (car diemtam) kctam) (cadr diemtam) ) )
  (setq diemdim1 (list (+ (car diemtam) kctam) (cadr diemtam))) ;diemdim1 cach diemtam =kctam
  (setq diemdim2 (list (car diemtam) (+ (cadr diemtam) 50))) ;;diemdim2 huong len tren truc y
  (setq diemdim3 (list (+ (+ (car diemtam) (* kctam (- sosong1 1))) dk) (cadr diemtam))) ;diemdim3 ngoai cung ben phai
  (setq diemdim4 (list (- (+ (car diemtam) (* kctam sosong1 )) dk) (cadr diemtam))) ;diemdim4
  (setq diemdim5 (list (- (- (car diemtam) (* kctam (- sosong1 1))) dk) (cadr diemtam))) ;diemdim5 ngoai cung ben phai
  (setq diemdim6 (list (+ (- (car diemtam) (* kctam sosong1 )) dk) (cadr diemtam)))
  (setq diemdim7 (list (+ (+ (car diemtam) (* kctam 2)) dk) (cadr diemtam))) ;diemdim5 ngoai cung ben phai
  (setq diemdim8 (list (- (+ (car diemtam) (* kctam 3)) dk) (cadr diemtam)))

  (setq diemtext (list (car diemtam) (+ (cadr diemtam) 200))) ;diemtext huong len tren truc y

  (command "dimlinear" diemtam diemdim1 diemdim2 )
  (command "dimlinear"  diemdim3 diemdim4 diemdim2)
  (command "dimlinear"  diemdim5 diemdim6 diemdim2)
  (command "dimlinear"  diemdim7 diemdim8 diemdim2)
  (command "text" diemtext 55 "0" (strcat (rtos sl) " SONG"))

  ;(command "osmode" 16383)
  (setvar "osmode" 16383)
  (print "so song la")
  (print sl)
  (princ)

)
;;;;;;;; end of VSS ;;;;;;;

;;;;;; START OF DIMFLAC ;;;;;;
(defun c:DFL025()
  (command "setvar" "dimlfac" "0.25" "")
  (princ))

(defun c:DFL1 ()
  (command "setvar" "dimlfac" "1" "")
  (princ))
;;;;;; END OF DIMFLAC ;;;;;;

;;;; START OF Copy and Fit X (CCFX) ;;;;;;
(defun c:CCFX ( / obj startPoint endPoint numCopies deltaX point2)
  (prompt "\nChọn đối tượng để copy: ")
  (setq obj (ssget))
  (if (not obj)
    (progn
      (prompt "\nKhông có đối tượng nào được chọn. Thoát lệnh.")
      (exit))
  )

  ;; Chọn điểm đầu
  (prompt "\nChọn điểm đầu: ")
  (setq startPoint (getpoint))
  (if (not startPoint)
    (progn
      (prompt "\nKhông chọn điểm đầu. Thoát lệnh.")
      (exit))
  )

  ;; Chọn điểm cuối
  (prompt "\nChọn điểm cuối: ")
  (setq endPoint (getpoint))
  (if (not endPoint)
    (progn
      (prompt "\nKhông chọn điểm cuối. Thoát lệnh.")
      (exit))
  )

  ;; Nhập số lượng đối tượng cần copy
  (setq numCopies (getint "\nNhập số lượng đối tượng cần copy: "))
  (if (or (not numCopies) (<= numCopies 1))
    (progn
      (prompt "\nSố lượng phải lớn hơn 1. Thoát lệnh.")
      (exit))
  )

  ;; Tính khoảng cách giữa các bản sao
  (setq deltaX (/ (- (car endPoint) (car startPoint)) numCopies))
  (setq point2 (list (+(car startPoint ) deltaX) (cadr endPoint)))

  ;; Copy đối tượng với khoảng cách phù hợp
  (command "COPY" obj "" startPoint "ARRAY" numCopies point2)

  (prompt "\nLệnh CopyFit hoàn tất.")
)
;;;;; End of CCFX ;;;;;



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
  ;(command "_.LAYER" "_SET" "_mss.phantom" "")
  (setvar "clayer" "_mss.phantom")
  ;(command "osmode" "5")
  (setvar "osmode" 5)
  ;; 1. Nhập tiền tố (ví dụ: "od_on_ol")
  (setq prefix (getstring "\nNhập tiền tố: "))
  (setq counter 1)
  (setq allText "")  ; chuỗi tích lũy toàn bộ định nghĩa

  ;; 2. Nhập các điểm
  (while (setq pt (getpoint (strcat "\nChọn điểm " (itoa counter) " (ESC/Enter để dừng): ")))
    ;; Vẽ hình tròn bán kính 2 tại điểm đã chọn
    (command "CIRCLE" pt "1")
    ;; Xác định vị trí đặt MTEXT cho định nghĩa của điểm:
    ;; đặt tại pt cộng thêm 1 đơn vị theo trục Y
    (setq ptText (list (car pt) (+ (cadr pt) 1) (if (numberp (caddr pt)) (caddr pt) 0)))
    
    (if (= counter 1)
      ;; Điểm đầu tiên: ví dụ: od_on_ol_p1 = APoint(od_on_ol_p1.x, od_on_ol_p1.y)
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
    
    ;; Vẽ MTEXT cho định nghĩa của điểm tại ptText.
    ;; Xác định hộp MTEXT bằng cách cộng thêm (190,2) vào ptText như yêu cầu.
    (setq secondPt (list (+ (car ptText) 190) (+ (cadr ptText) 2)))
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
      (setq secondPt (list (+ (car finalPt) 190) (+ (cadr finalPt) 2)))
      (command "MTEXT" finalPt secondPt allText "")
    )
  )
  ;(command "osmode" "16383")
  (setvar "osmode" 16383)
  (princ)
)

(princ "\nGõ VE_DIEM để chạy chương trình.\n")
(princ)
;;;;;; End VVD ;;;;;;

;;;;; START of VIEWS ;;;;;;

(defun c:v2v()
(command "VPORTS" "2" "V")
)

(defun c:v2h()
(command "VPORTS" "2" "H")
)

(defun c:1v()
(command "VPORTS" "SI" )
)
;;;;; END of VIEWS ;;;;;;

