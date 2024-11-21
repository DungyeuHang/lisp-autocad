;Lệnh line = 1
(defun c:1 ()
(command "line")
)

;Lệnh move  = 2
(defun c:2 ()
    (command "move")
)

;Lệnh insert = 3 không dùng được lisp


;Lệnh layon = 4
(defun c:4 ()
    (command "layon")
)

;Lệnh layoff = 5
(defun c:5 ()
    (command "layoff")
)

;lệnh mocoro -11
(defun c:11 ()
    (command "mocoro")
)

;lệnh mirror -33
(defun c:33 ()
    (command "mirror")
)

;lệnh matchprop -44
(defun c:44 ()
    (command "matchprop")
)

;lệnh copy -cc không lặp lại nhiều lần


;lệnh quickselect- qq
(defun c:qq ()
    (command "qselect")
)

;lệnh filter - ff
(defun c:ff ()
    (command "filter")
)

;lệnh dimlinear -dd
(defun c:dd ()
    (command "dimlinear")
)

;lệnh find-f1
(defun c:f1 ()
    (command "find")
)

;lệnh dimspace -dss
(defun c:dss ()
    (command "dimspace")
)

;lệnh dimcontinue -dcc
(defun c:dcc ()
    (command "dimcontinue")
)

;Center mid mode c1
(defun c:c1()
  (command "osmode" "1030")
  (princ)
)
;end of c1


;Copy and rotate -CR
( defun c:cr(/ diemdau diemden dt)
  (setq dt (ssget))
  (setq diemdau (getpoint "chon diem dau" )) ;chon diem dau
  (setq diemden (getpoint "chon diem den" )) ;chon diem den
  (command "mocoro" dt "" diemdau "copy" diemden "" "rotate" "" diemden)
 )
;end of CR

;Copy to mid and erase cs -CES
(defun c:CES (/ dt diemdau ktc kt d1 d2 )
  (command "osmode" 16384)
  (setq diemdau (list 0 0))
  (setq ktc (getreal "\nnhap kich thuoc canh chinh"))
  (setq kt (+ ktc 5.5))
  (setq dt (ssget))
  (setq d1 (list (+ (car diemdau) kt)(cadr diemdau)))
  (setq d2 (list (- (car diemdau) kt)(cadr diemdau)))
  (command "copy" dt ""  diemdau d1 "")
  (command "copy" dt ""  diemdau d2 "")
 (command "erase" dt "")
  (command "osmode" 16383)
  (princ)
)
;end of CES

;Dungx insert -dungx
(defun c:dungx (/ pt time date-string year month day formatted-date new-pt)
  ;; Nhập điểm chèn từ người dùng
  (setq pt (getpoint "\nChọn điểm chèn: "))
  
  ;; Chèn khối SIGNATURE tại điểm chèn
  (command "insert" "SIGNATURE" pt "" "")

 ;; Lấy giá trị thời gian từ AutoCAD
  (setq time (RTOS (GETVAR "Cdate")))

;;;;;;;;;;;;;;;;;

;; Tách chuỗi thành phần năm, tháng, ngày (8 ký tự)
  (setq date-string (substr time 1 8))  ;; Lấy 8 ký tự đầu tiên (yyyyMMdd)

  ;; Tách năm, tháng, ngày
  (setq year (substr date-string 1 4))     ;; Năm: 2024
  (setq month (substr date-string 5 2))    ;; Tháng: 10
  (setq day (substr date-string 7 2))       ;; Ngày: 17

  ;; Tạo chuỗi văn bản để hiển thị theo định dạng Ngày: DD/MM/YYYY
  (setq formatted-date (strcat day "/" month "/" year))
;;;;;;;;;;;;;;;;;


  ;; Tạo một điểm mới để hiển thị ngày (dời xuống 100 đơn vị theo trục Y)
  (setq new-pt (list (-(car pt) 200) (+ (cadr pt) -150)))

  ;; Hiển thị chuỗi văn bản tại điểm đã chỉ địnH
  (command "text" new-pt 55 "0" formatted-date )
(princ)
)
;end of DUNGX

;full osmode -SS
( defun c:ss()
 (command "osmode" "16383")
  (princ)
 )
;end of SS

;Keo Canh Lech danh cho cua 4 canh - KCL
( defun c:KCL(/ d1 diemtam ktk doituong1 doituong2 doituong3)
  (command "osmode" 16384)
  (setq d1 (list 0 0))
  (setq ktk (getreal "\nhap kich thuoc keo"))
  (setq doituong1 (ssget))
  (command "stretch" doituong1 "" d1 (list (- (car d1) ktk) (cadr d1) ) "")
  (setq doituong2 (ssget))
  (command "stretch" doituong2 "" d1 (list (+ (car d1) ktk) (cadr d1) ) "")
  (setq doituong3 (ssget))
  (command "stretch" doituong3 "" d1 (list (car d1) (+ (cadr d1) ktk ) ) "")
  (command "osmode" 16383)   
  (princ)
 )
;end of KCL

;Keo cua so 3 canh -KCS3
(defun c:kcs3 (/ d1 kto dk kt1 kt4 ktk doituongx doituongm doituong1 doituong2 doituong3)
  (command "osmode" "16384")
  (setq d1 (list 0 0))
  (setq kto (getreal "\nnhap kich thuoc o cho"))
  (setq dk (getreal "\nnhap kich thuoc DAY KHUON"))
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
  (command "osmode" "16383")   
  (princ)
  )
;end of KCS3

;LayOff 5 layer de lam phoi -OOO
(defun c:ooo()
       (command "layoff" "" "layer" "off" "_mss.dut" "" "Y" "" )
       (command "layoff" "" "layer" "off" "_mss.cat" "" "Y" "" )
  	(command "layoff" "" "layer" "off" "_mss.phantom" "" "Y" "" )
  	(command "layoff" "" "layer" "off" "_mss.pro" "" "Y" "" )
       (command "layoff" "" "layer" "off" "DUNGX" "" "Y" "" )
(princ)
  )
;end of OOO

;set last -LL
(defun c:ll ()
  (setq l (getreal "nhap so can set cho L"))
  (setq k (/ l 2))
 )
;end of LL

;Stretch X -SX
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
  (command "osmode" "16384")
 (repeat 10
(setq doituong1 (ssget))
  (setq doituong2 (ssget))
  (command "stretch" doituong1 "" d1 (list (- (car d1) (* hs b)) (cadr d1) ) "") ;(command "stretch" )
  (command "stretch" doituong2 "" d1 (list (+ (car d1) (* hs b)) (cadr d1) ) "")
)
  (command "osmode" "16383")
)
;end of SX

;Stretch Y -SY
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
  (command "osmode" "16384")
  (setq d1 (list 0 0))
(repeat 10
  (setq doituong1 (ssget))
  (setq doituong2 (ssget))
  (command "stretch" doituong1 "" d1 (list (car d1) (+ (cadr d1) (* hs b)) ) "") ;(command "stretch" )
  (command "stretch" doituong2 "" d1 (list (car d1) (- (cadr d1) (* hs b)) ) "")
)
  (command "osmode" "16383")
  )
;end of SY

;Ve lo song -VLS
(defun c:vls(/ dd dt diemtam doituong sosong1 kctam)
  (command "osmode" "16384")
  (setq dd (getreal "\nnhap chieu dai do"))
  (setq dt (+ (/ dd 2) 70)) ;chieu dai tinh
  (setq diemtam (list 0 0))
  (setq doituong (ssget))
  (setq sosong1 (+ (fix (/ dt 100 ) ) 1) )
  (setq kctam (/ dt sosong1))
  (command "copy" doituong "" diemtam "_array" sosong1 (list (+ (car diemtam) kctam) (cadr diemtam) ) )
  (command "copy" doituong "" diemtam "_array" sosong1 (list (- (car diemtam) kctam) (cadr diemtam) ) )
  (command "osmode" "16383")
  (princ)
)
;end of VLS

;VE SONG 3 KIEU -VSS
(defun c:vss (/ dd dk keyword diemtam doituong sls_1 sls_test tam_lo_song kctam sl sosong1 diemdim1 diemdim2 diemdim3 diemdim4 diemdim5 diemdim6 diemdim7 diemdim8 diemtext)
(command "osmode" "16383")
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
  
  (command "osmode" "16384")
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

  (command "osmode" 16383)
  (print "so song la")
  (print sl)
  (princ)

)
;end of VSS