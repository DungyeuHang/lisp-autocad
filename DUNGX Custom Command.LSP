;Lệnh line = 1
(defun c:1 ( / lay )
  (prompt "\n🚀 Đang xử lý...")
  (setvar "osmode" 16383)

  ;; Kiểm tra layer "_mss.bao" có tồn tại
  (setq lay (tblsearch "layer" "_mss.bao"))
  (if lay
    (progn
      (setvar "clayer" "_mss.bao")
      (prompt "\n✅ Đã chuyển sang layer _mss.bao.")
    )
    (prompt "\n⚠️ Layer _mss.bao không tồn tại, vẫn tiếp tục vẽ.")
  )

  (command "line")
  (princ)
)



;Lệnh move  = 2
(defun c:2 ()
;(command "osmode" 16383)
  (setvar "osmode" 16383)
  (prompt "\n🚀 Đang xử lý...")
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
  (prompt "\n🚀 Đang xử lý...")
  (setvar "osmode" 16383)
  (command "mirror"))

;lệnh matchprop -44
(defun c:44 ()
  (prompt "\n🚀 Đang xử lý...")
  (command "matchprop"))

;lệnh copy -cc không lặp lại nhiều lần


;lệnh quickselect- qq
(defun c:qq ()
  (prompt "\n🚀 Đang xử lý...")
  (command "qselect"))

;lệnh filter - ff
(defun c:ff ()
  (prompt "\n🚀 Đang xử lý...")
  (command "filter"))

;lệnh dimlinear -dd
(defun c:dd ( / lay)
  (prompt "\n🚀 Đang xử lý...")
  (setvar "osmode" 16383)
  ;; Kiểm tra layer "_mss.bao" có tồn tại
  (setq lay (tblsearch "layer" "_mss.kichthuoc"))
  (if lay
    (progn
      (setvar "clayer" "_mss.kichthuoc")
      (prompt "\n✅ Đã chuyển sang layer _mss.kichthuoc.")
    )
    (prompt "\n⚠️ Layer _mss.kichthuoc không tồn tại, vẫn tiếp tục vẽ.")
  )
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
  (prompt "\n🚀 Đang xử lý...")
  (setvar "clayer" "_mss.phantom")
  (setq i 1) ;; Bắt đầu đếm từ 1
  ;(command "osmode" "16384")
  (setvar "osmode" 16384)

  ;; Vòng lặp vô hạn, chỉ dừng khi người dùng nhấn Enter hoặc Space
  (while (setq pt (getpoint "\n💬 Chọn điểm chèn (Nhấn Enter để thoát): "))
    
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
    (prompt "\n🚀 Đã xử lý...")
    ;; Tăng giá trị đếm
    (setq i (1+ i))
  )

  ;; Khi thoát vòng lặp, chạy các lệnh cuối
  ;(command "osmode" "16383")
  (setvar "osmode" 16383)
  ;(command "_.LAYER" "_SET" "_mss.kichthuoc" "")
  (setvar "clayer" "_mss.kichthuoc")
  (prompt "\n✅ ĐÃ XỬ LÝ XONG.")
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
  (prompt "\n🚀 Đang xử lý...")
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
  (prompt "\n✅ ĐÃ XỬ LÝ XONG.")  
  (princ)
 )
;;;;;;;;;;;; end of KCL ;;;;;;;;;;




;     <<<<<<<<<< Keo cua so 3 canh -KCS3 >>>>>>>>>>>> ;
(defun c:kcs3 (/ d1 kto dk kt1 kt4 ktk doituongx doituongm doituong1 doituong2 doituong3)
  (prompt "\n🚀 Đang xử lý...")
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
  (prompt "\n✅ ĐÃ XỬ LÝ XONG.")
  (princ)
  )
;;;;;;;;; end of KCS3 ;;;;;;;;




;          <<<<<<<< LayOff 5 layer de lam phoi -OOO >>>>>>>> ;
(defun c:ooo ()
  (prompt "\n🚀 Đang xử lý...")
  (setq doc (vla-get-activedocument (vlax-get-acad-object))) ;; Lấy tài liệu hiện tại
  (setq layman (vla-get-layers doc)) ;; Lấy danh sách layer

  ;; Danh sách các layer cần tắt
  (setq layers '("_mss.dut" "_mss.cat" "_mss.phantom" "_mss.pro" "_mss.canh" "DUNGX"))

  ;; Lặp qua từng layer và tắt nếu tồn tại
  (foreach layer layers
    (setq lay (vl-catch-all-apply 'vla-item (list layman layer))) ;; Kiểm tra layer có tồn tại không
    (if (and lay (not (vl-catch-all-error-p lay))) ;; Nếu layer tồn tại
      (vla-put-layeron lay :vlax-false) ;; Tắt layer
    )
  )
(prompt "\n✅ ĐÃ XỬ LÝ XONG.")
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
  (prompt "\n🚀 Đang xử lý...")
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
  (prompt "\n✅ ĐÃ XỬ LÝ XONG.")
)
;;;;;   end of SX   ;;;;;




;         <<<<<< Stretch Y -SY >>>>>>> ;
(defun c:SY (/ a b keyword hs d1 doituong1 doituong2)
  (prompt "\n🚀 Đang xử lý...")
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
  (prompt "\n🚀 Đang xử lý...")
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
  (prompt "\n✅ ĐÃ XỬ LÝ XONG.")
  (princ)
)
;;;;;;;;;;;; end of VLS ;;;;;;;;

;       <<<<<<<<<<< VE SONG 3 KIEU -VSS >>>>>>>>> ;
(defun c:vss (/ dd dk keyword diemtam doituong sls_1 sls_test tam_lo_song kctam sl sosong1 diemdim1 diemdim2 diemdim3 diemdim4 diemdim5 diemdim6 diemdim7 diemdim8 diemtext)
(prompt "\n🚀 Đang xử lý...")
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
  (prompt "\n✅ ĐÃ XỬ LÝ XONG.")
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

(defun c:APOINT (/ ent obj coords index ptList prefix count pt lastPt defLine allText ptText ptNext startPt startIndex closestDist i tmpPt finalPt finalNext dx dy dxStr dyStr)
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
      (setq startPt (getpoint "\n🧭 Chọn điểm đầu tiên để bắt đầu đánh số: "))
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
      (setq textInsertPt (getpoint "\n📌 Chọn điểm để gán text create_smart_shape: "))
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
          (cons 40 1.0)
          (cons 1 smartShapeStr)
        )
      )
)

    (prompt "\n❌ Đối tượng không phải Polyline!"))

  (princ))


;; <<<<<<<<<<<<<<<<<<<        End of  Make Points by Polyline      >>>>>>>>>>>>>>>>>


;;;;;;;;;;;;;;;;;;; Start of change the first point of polyline ;;;;;;;;;;;;;;

(defun nthcdr (n lst)
  (if (or (<= n 0) (null lst))
    lst
    (nthcdr (1- n) (cdr lst))))

(defun take (lst n)
  (if (or (null lst) (= n 0)) '() (cons (car lst) (take (cdr lst) (1- n)))))

(defun rotate-list (lst n)
  (append (nthcdr n lst) (reverse (reverse (take lst n)))))

;Đổi điểm đầu polyline
(defun c:RTPL (/ ent obj coords ptList index pt startPt closestDist i newCoords startIndex bulges)

  (setq ent (car (entsel "\n🎯 Chọn polyline cần đổi điểm đầu: ")))
  (if (and ent (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE"))
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (setq coords (vlax-get obj 'Coordinates))
      (setq ptList '())
      (setq index 0)
      ;; Tạo danh sách điểm từ coords
      (while (< index (length coords))
        (setq pt (list (nth index coords) (nth (+ index 1) coords)))
        (setq ptList (append ptList (list pt)))
        (setq index (+ index 2)))

      ;; 🔁 Lấy bulge gốc trước khi sửa
      (setq bulges '())
      (setq i 0)
      (repeat (/ (length coords) 2)
        (setq bulges (append bulges (list (vla-GetBulge obj i))))
        (setq i (1+ i)))

      ;; Chọn điểm tham chiếu
      (setq startPt (getpoint "\n📌 Chọn điểm muốn đặt làm điểm đầu: "))

      ;; Tìm chỉ số điểm gần nhất
      (setq closestDist 1e99)
      (setq i 0)
      (foreach tmpPt ptList
        (if (< (distance tmpPt startPt) closestDist)
          (progn
            (setq closestDist (distance tmpPt startPt))
            (setq startIndex i)))
        (setq i (1+ i)))

      ;; Xoay danh sách điểm và bulge theo điểm đầu mới
      (setq ptList (rotate-list ptList startIndex))
      (setq bulges (rotate-list bulges startIndex))

      ;; Gán lại vào polyline
      (setq newCoords (apply 'append ptList))
      (vlax-put obj 'Coordinates newCoords)

      (setq i 0)
      (foreach b bulges
        (vla-SetBulge obj i b)
        (setq i (1+ i)))

      (prompt "\n✅ Đã đổi điểm đầu polyline và giữ nguyên cung."))
    (prompt "\n❌ Không phải là LWPOLYLINE!"))
  (princ))


;;;;;;;;;;;;;;;;;;;; End of change the first point of polyline ;;;;;;;;;;;;;;









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

