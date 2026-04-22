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
(defun c:55 ()
  ;(command "osmode" 16383)
  (setvar "osmode" 16383)
(command "mocoro"))


;lệnh matchprop -44
(defun c:44 ()
  (prompt "\n🚀 Đang xử lý...")
  (command "matchprop"))



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
  (command "dimlinear"))

;lệnh find-f1
(defun c:f1 ()
    (command "find"))

;lệnh COPY-CC
;(defun c:cc ()
    ;(command "copy" ))

;lệnh mirror-33
(defun c:33 ()
    (command "mirror"))

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


;;;;;;;;;;;;;;;;;;  < Dungx auto insert SIGNATURE by frame > ;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  DUNGX – AUTO INSERT SIGNATURE BY FRAME (FINAL)         ;;
;;  Sort order: TOP → DOWN, LEFT → RIGHT                  ;;
;;  Y : DESCENDING                                         ;;
;;  X : ASCENDING                                          ;;
;;  Frame layer : _mss.in                                  ;;
;;  Text  layer : _mss.phantom                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(vl-load-com)

;; ---------------------------------------------------------
;; Create text helper
(defun create-text (point text-string text-height text-layer
                    / document modelspace text-object)
  (setq document   (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq modelspace (vla-get-ModelSpace document))
  (setq text-object
        (vla-AddText
          modelspace
          text-string
          (vlax-3d-point point)
          text-height
        )
  )
  (vla-put-Layer text-object text-layer)
  text-object
)

;; ---------------------------------------------------------
(defun sort-frames-left-to-right (ss / idx ent maxpt lst)
  (setq idx 0 lst '())

  (while (< idx (sslength ss))
    (setq ent (ssname ss idx))
    (vla-getBoundingBox
      (vlax-ename->vla-object ent)
      'minpt
      'maxpt
    )
    (setq maxpt (vlax-safearray->list maxpt))

    ;; lưu (Xmax . entity)
    (setq lst (cons (cons (car maxpt) ent) lst))
    (setq idx (1+ idx))
  )

  ;; sort X tăng dần (trái → phải)
  (vl-sort lst '(lambda (a b) (< (car a) (car b))))
)




;       <<<<<<<< full osmode -SS >>>>>>>          ;
( defun c:SSS_FULL_OS_MODE()
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
(defun ll-ssnewer (ent / ss e)
  (setq e (if ent (entnext ent) (entnext)))
  (while e
    (if (null ss)
      (setq ss (ssadd))
    )
    (setq ss (ssadd e ss))
    (setq e (entnext e))
  )
  ss
)

(defun ll-ignore-command-p (cmd / up)
  (setq up (strcase cmd))
  (or (= up "LL")
      (= up "LLON")
      (= up "LLOFF"))
)

(defun ll-reactor-start (reactor params / cmd)
  (setq cmd (car params))
  (if (not (ll-ignore-command-p cmd))
    (setq *ll-marker-stack* (cons (entlast) *ll-marker-stack*))
  )
)

(defun ll-reactor-ended (reactor params / cmd marker ss)
  (setq cmd (car params))
  (if (not (ll-ignore-command-p cmd))
    (progn
      (setq marker (car *ll-marker-stack*))
      (setq *ll-marker-stack* (cdr *ll-marker-stack*))
      (setq ss (ll-ssnewer marker))
      (setq *ll-last-batch* ss)
      (setq *ll-last-command* (strcase cmd))
    )
  )
)

(defun ll-reactor-cancelled (reactor params / cmd)
  (setq cmd (car params))
  (if (not (ll-ignore-command-p cmd))
    (progn
      (if *ll-marker-stack*
        (setq *ll-marker-stack* (cdr *ll-marker-stack*))
      )
      (setq *ll-last-batch* nil)
      (setq *ll-last-command* (strcase cmd))
    )
  )
)

(defun ll-ensure-reactor ( )
  (if (or (null *ll-reactor*)
          (not (vlr-added-p *ll-reactor*)))
    (setq *ll-reactor*
      (vlr-command-reactor
        nil
        '(
          (:vlr-commandWillStart . ll-reactor-start)
          (:vlr-commandEnded . ll-reactor-ended)
          (:vlr-commandCancelled . ll-reactor-cancelled)
          (:vlr-commandFailed . ll-reactor-cancelled)
        )
      )
    )
  )
  *ll-reactor*
)

(defun c:WW-SET-LAST ( / )
  (ll-ensure-reactor)
  (setq l *ll-last-batch*)
  (if (and l (> (sslength l) 0))
    (progn
      (sssetfirst nil l)
      (prompt
        (strcat
          "\nDa luu "
          (itoa (sslength l))
          " doi tuong moi cua lenh "
          (if *ll-last-command* *ll-last-command* "?")
          " vao bien L. Go !L de dung lai."
        )
      )
    )
    (prompt "\nKhong tim thay batch doi tuong moi cua lenh gan nhat.")
  )
  (princ)
)

(defun c:LLON ( / )
  (ll-ensure-reactor)
  (prompt "\nLL tracking da bat.")
  (princ)
)

(defun c:LLOFF ( / )
  (if (and *ll-reactor* (vlr-added-p *ll-reactor*))
    (vlr-remove *ll-reactor*)
  )
  (setq *ll-reactor* nil)
  (prompt "\nLL tracking da tat.")
  (princ)
)

(ll-ensure-reactor)
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
  ;(setq dd (getreal "\nnhap chieu dai do"))
  (while (not dd) ; lặp đến khi có giá trị chiều dài
    (setq ent (car (entsel "\nChon doi tuong DIM: ")))
    (if ent
      (if (= (cdr (assoc 0 (entget ent))) "DIMENSION")
        (progn
          (setq dd (vla-get-Measurement (vlax-ename->vla-object ent)))
          (princ (strcat "\nDo dai DIM: " (rtos dd 2 2)))
        )
        (prompt "\n❌ Khong phai DIM. Vui long chon lai.")
      )
      (prompt "\n⚠️ Khong chon gi ca. Vui long chon lai.")
    )
  )
  
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
(defun c:vss (/ iiii ent1 dd dk keyword diemtam doituong sls_1 sls_test tam_lo_song kctam sl sosong1 diemdim1 diemdim2 diemdim3 diemdim4 diemdim5 diemdim6 diemdim7 diemdim8 diemtext ename ent doituong1 bbox minPt maxPt diemtam themObj)
(command "osmode" "16383")
(initget 1 "tr th v") ;tron thoi vuong
  (setq keyword (getkword "Ban muon ve song tron <tr>, thoi <th> hay song vuong <v>"))
  (cond
    ((= keyword "tr") ;tron r=17.25
     (setq dk 17.6)) ; ket thuc dk1
    ((= keyword "th") ;thoi r= 20.75
     (setq dk 20.75 )) ; ket thuc dk2
    ((= keyword "v") ;vuong  = 15.5
     (setq dk 15.5 )) ; ket thuc dk3
     ) ;ket thuc cond
  (while (not dd) ; lặp đến khi có giá trị chiều dài
    (setq ent1 (car (entsel "\nChon doi tuong DIM: ")))
    (if ent1
      (if (= (cdr (assoc 0 (entget ent1))) "DIMENSION")
        (progn
          (setq dd (vla-get-Measurement (vlax-ename->vla-object ent1)))
          (princ (strcat "\nDo dai DIM: " (rtos dd 2 2)))
        )
        (prompt "\n❌ Khong phai DIM. Vui long chon lai.")
      )
      (prompt "\n⚠️ Khong chon gi ca. Vui long chon lai.")
    )
  )
  

; Bước 1: Chọn đối tượng đầu tiên để lấy tâm
  (prompt "\nChọn đối tượng đầu tiên để xác định tâm...")
  (setq doituong1 (ssget "_+.:S")) ; chọn 1 đối tượng

  (if (and doituong1 (= (sslength doituong1) 1))
    (progn
      (setq ename (ssname doituong1 0))
      (setq ent (entget ename))
      (setq type1 (cdr (assoc 0 ent))) ; lấy loại đối tượng

      ;; Xác định tọa độ tâm dựa theo loại đối tượng
      (cond
        ((= type1 "CIRCLE")
         (setq diemtam (cdr (assoc 10 ent)))
        )
        ((= type1 "INSERT") ; BLOCK
         (setq diemtam (cdr (assoc 10 ent))) ; điểm chèn block
        )
        ((= type1 "LWPOLYLINE")
         (setq bbox (vla-getboundingbox (vlax-ename->vla-object ename) 'minPt 'maxPt))
         (setq minPt (vlax-safearray->list minPt))
         (setq maxPt (vlax-safearray->list maxPt))
         (setq diemtam (mapcar '(lambda (a b) (/ (+ a b) 2.0)) minPt maxPt)) ; tâm bbox
        )
        (T (prompt "\nLoại đối tượng không được hỗ trợ."))
      )

      ;; In tọa độ tâm ra màn hình
      (if diemtam
        (progn
          (princ (strcat "\nTâm của đối tượng là: "
                         (rtos (car diemtam) 2 3) ", "
                         (rtos (cadr diemtam) 2 3)))

          ;; Bước 2: Chọn thêm các đối tượng khác (tùy chọn)
          (prompt "\nChọn các đối tượng khác (nếu có), nhấn ENTER nếu không có...")
          (setq themObj (ssget)) ; chọn nhiều đối tượng hoặc bỏ qua

          ;; Gộp doituong1 và các đối tượng thêm vào thành 1 selection set mới
          (cond
            (themObj
              ;; Tạo selection set mới chứa cả doituong1 và themObj
              (setq doituong (ssadd (ssname doituong1 0)))
              (repeat (sslength themObj)
                (setq doituong (ssadd (ssname themObj (setq iiii (if iiii (1+ iiii) 0))) doituong))
              )
            )
            (T
              (setq doituong doituong1)
            )
          )

          ;; Bạn có thể tiếp tục xử lý với tập `doituong` ở đây
          (princ (strcat "\nTổng số đối tượng đã chọn: " (itoa (sslength doituong))))
        )
      )
    )
    (prompt "\nVui lòng chọn đúng 1 đối tượng đầu tiên.")
  )
  (foreach sls_1 (list 21 19 17 15 13 11 9 7 5 3 )
          (setq sls_test (- sls_1 2))
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



;;;;;;;;;;;;;;;;;;; Start of JoinDimAuto ;;;;;;;;;;;;;;;;;;;;;;
(defun c:JoinDimAuto ()
  (prompt "\n>> Chọn các DIM muốn gộp (tối thiểu 2): ")
  (setq ss (ssget '((0 . "DIMENSION"))))
  (if (and ss (>= (sslength ss) 2))
    (progn
      (setq allpts '()
            firstEnt (entget (ssname ss 0))
            firstP1 (cdr (assoc 13 firstEnt))
            firstP2 (cdr (assoc 14 firstEnt))
            firstTextPos (cdr (assoc 11 firstEnt))) ; vị trí chữ của dim đầu

      ;; Xác định hướng: ngang hay dọc
      (if (> (abs (- (car firstP1) (car firstP2))) (abs (- (cadr firstP1) (cadr firstP2))))
        (setq isHorizontal T)
        (setq isHorizontal nil))

      ;; Tính khoảng offset vuông góc từ đoạn dim đến chữ (dùng khoảng cách vuông góc)
      (if isHorizontal
        (setq offset (- (cadr firstTextPos) (cadr firstP1))) ; theo trục Y
        (setq offset (- (car firstTextPos) (car firstP1)))) ; theo trục X

      ;; Gom tất cả điểm
      (repeat (sslength ss)
        (setq ent (entget (ssname ss 0)))
        (setq ss (ssdel (ssname ss 0) ss))

        (setq p1 (cdr (assoc 13 ent)))
        (setq p2 (cdr (assoc 14 ent)))
        (setq allpts (append allpts (list p1 p2)))

        (entdel (cdr (assoc -1 ent))) ; xóa DIM cũ
      )

      ;; Tìm điểm min/max theo hướng chính
      (if isHorizontal
        (progn
          (setq sorted (vl-sort allpts (function (lambda (a b) (< (car a) (car b))))))
          (setq ptStart (car sorted))
          (setq ptEnd (car (reverse sorted)))
          ;; tính điểm dim theo trục Y
          (setq dimPt (list (car ptStart) (+ (cadr ptStart) offset) 0.0))
        )
        (progn
          (setq sorted (vl-sort allpts (function (lambda (a b) (< (cadr a) (cadr b))))))
          (setq ptStart (car sorted))
          (setq ptEnd (car (reverse sorted)))
          ;; tính điểm dim theo trục X
          (setq dimPt (list (+ (car ptStart) offset) (cadr ptStart) 0.0))
        )
      )

      ;; Vẽ dim mới
      (command "_.DIMLINEAR" "_non" ptStart "_non" ptEnd "_non" dimPt "")

      (princ "\n✅ Đã gộp DIM và đặt đúng vị trí.")
    )
    (prompt "\n⚠️ Phải chọn ít nhất 2 DIMENSION.")
  )
  (princ)
)

;;;;;;;;;;;;;;;;;;; End of JoinDimAuto ;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;; End of CDD-CHIADIM ;;;;;;;;;;;;;;;;;;;




;***************************************************************************************************************************************************************************************************************
;                                 START ............................... VAA VE ARC -VAULT ..................................... =========>;

(defun c:vaa-ve-arc ( / step get-dim-measurement make-rect add-dim ent meas inspt)
  (vl-load-com)
  (setq step 1)
  (setvar "osmode" 16384)
  
  (defun get-dim-measurement (ent / entObj objType measurement)
  (if (and ent (setq entObj (vlax-ename->vla-object ent)))
    (progn
      (setq objType (vla-get-ObjectName entObj))
      (cond
        ;; Dimension
        ((wcmatch objType "*Dimension")
         (setq measurement (vla-get-Measurement entObj))
        )
        ;; Line
        ((= objType "AcDbLine")
         (setq measurement
               (distance
                 (vlax-get entObj 'StartPoint)
                 (vlax-get entObj 'EndPoint)
               )
         )
        )
        ;; Polyline (2D/3D/LWPolyline)
        ((wcmatch objType "AcDb*Polyline")
         (setq measurement 
               (vlax-curve-getDistAtParam ent (vlax-curve-getEndParam ent))
         )
        )
        ;; Arc
        ((= objType "AcDbArc")
         (setq measurement (vla-get-ArcLength entObj))
        )
        ;; Circle (thêm luôn cho đủ bộ)
        ((= objType "AcDbCircle")
         (setq measurement (* 2 pi (vla-get-Radius entObj)))
        )
      )
    )
  )
    measurement
  )



  ;; Hàm tạo RECTANG + ghi dim
  (defun make-rect (base-pt len width)
    (setq x (car base-pt))
    (setq y (cadr base-pt))
    (setq p1 (list x y))                             ; góc trái dưới
    (setq p2 (list (+ x len) y))                     ; góc phải dưới
    (setq p3 (list (+ x len) (+ y width)))           ; góc phải trên
    (setq p4 (list x (+ y width)))                   ; góc trái trên

    ;; Vẽ RECTANG
    (setvar "clayer" "_mss.bao")
    (command "RECTANG" p1 p3)
    (setvar "clayer" "_mss.kichthuoc")
 ;; Tạo DIM chiều dài: dưới hình chữ nhật
    (command "DIMLINEAR" p1 p2 (list x (- y 100)) "")

    ;; Tạo DIM chiều rộng: bên phải hình chữ nhật
    (command "DIMLINEAR" p4 p1 (list (- x  100)  y ) "")
  )
  

  ;; Vòng lặp 2 lần
  (repeat 2
    (prompt (strcat "\nLần " (itoa step) ": Chọn đối tượng bất kỳ (DIM, PLINE, LINE)..."))
    (setq ent (car (entsel)))
    (if (and ent (setq meas (get-dim-measurement ent)))
      (progn
        (setq meas (+ meas 0.2))
        (prompt (strcat "\nChiều dài đo được: " (rtos meas 2 2)))
        (setq inspt (getpoint "\nChọn điểm đặt RECTANG: "))
        (if (= step 1)
          (make-rect inspt meas 48.8)
          (make-rect inspt meas 18.8)
        )
        (setq step (+ step 1))
      )
      (prompt "\n❌ Không phải DIM hợp lệ hoặc không đo được.")
    )
  )
  (setvar "osmode" 16383)

  (princ)
)

;***************************************************************************************************************************************************************************************************************
;                                          <=============.............................. UN-FILLET ......................................... END;
;***************************************************************************************************************************************************************************************************************




;***************************************************************************************************************************************************************************************************************
;                                 START ............................... CTC-CENTER TO CIRCLE ..................................... =========>;
;;; =======================
;;; Copy From Source Center To Centers of Selected Circles
;;; Command: CTC
;;; Phiên bản: không dùng biến 1 ký tự (i, n, d, v.v.)
;;; =======================

(defun c2c-mid (ptA ptB)
  (mapcar '(lambda (a b) (/ (+ a b) 2.0)) ptA ptB))

(defun c2c-bbox-center (ent-name / vla-obj sa-min sa-max pt-min pt-max)
  (if (setq vla-obj (vlax-ename->vla-object ent-name))
    (progn
      (vla-getboundingbox vla-obj 'sa-min 'sa-max)
      (setq pt-min (vlax-safearray->list sa-min)
            pt-max (vlax-safearray->list sa-max))
      (c2c-mid pt-min pt-max)
    )
  )
)

(defun c2c-obj-center (ent-name / ent-data ent-type)
  (setq ent-data (entget ent-name)
        ent-type (cdr (assoc 0 ent-data)))
  (cond
    ((member ent-type '("CIRCLE" "ARC" "ELLIPSE" "POINT"))
      (cdr (assoc 10 ent-data)))                 ; tâm/điểm
    ((= ent-type "LINE")
      (c2c-mid (cdr (assoc 10 ent-data)) (cdr (assoc 11 ent-data))))
    ((= ent-type "INSERT")
      (cdr (assoc 10 ent-data)))                 ; điểm chèn block
    ((member ent-type '("LWPOLYLINE" "POLYLINE" "SPLINE" "REGION"))
      (c2c-bbox-center ent-name))                ; tâm bbox (xấp xỉ)
    (T (c2c-bbox-center ent-name))
  )
)

(defun C:CTC ( / c2c-old-cmdecho c2c-picked c2c-entity-src c2c-source-center
                     c2c-ss-circles c2c-idx c2c-count
                     c2c-entity-cir c2c-dest-center)

  (defun *error* (c2c-msg)
    (if (and c2c-msg (not (wcmatch (strcase c2c-msg) "*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\n[!] Lỗi: " c2c-msg))
    )
    (if c2c-old-cmdecho (setvar 'CMDECHO c2c-old-cmdecho))
    (command "_.UNDO" "_END")
    (princ)
  )

  (setq c2c-old-cmdecho (getvar 'CMDECHO))
  (setvar 'CMDECHO 0)
  (command "_.UNDO" "_GROUP")

  ;; 1) Chọn đối tượng nguồn
  (prompt "\nChọn đối tượng 1 cần COPY: ")
  (while (not (setq c2c-picked (entsel)))
    (prompt "\n[!] Hãy chọn 1 đối tượng.")
  )
  (setq c2c-entity-src (car c2c-picked))

  ;; 2) Lấy tâm nguồn (auto → fallback chọn tay)
  (setq c2c-source-center (c2c-obj-center c2c-entity-src))
  (if (not (and c2c-source-center (= (length c2c-source-center) 3)))
    (progn
      (prompt "\nKhông xác định được tâm tự động. Hãy chỉ điểm tâm nguồn.")
      (setq c2c-source-center (getpoint "\nChọn tâm nguồn: "))
    )
  )
  (if (not c2c-source-center) (progn (*error* "Không có tâm nguồn.") (exit)))

  ;; 3) Quét chọn các CIRCLE đích
  (prompt "\nChọn các CIRCLE đích (quét vùng, nhiều lần được): ")
  (setq c2c-ss-circles (ssget "_:L" '((0 . "CIRCLE"))))
  (if (not c2c-ss-circles)
    (progn
      (prompt "\n[!] Không có CIRCLE nào được chọn.")
      (*error* nil)
      (exit)
    )
  )

  ;; 4) Lặp copy tới từng tâm CIRCLE
  (setq c2c-count (sslength c2c-ss-circles)
        c2c-idx   0)
  (while (< c2c-idx c2c-count)
    (setq c2c-entity-cir (ssname c2c-ss-circles c2c-idx)
          c2c-dest-center (cdr (assoc 10 (entget c2c-entity-cir))))
    (if (and c2c-dest-center (not (equal c2c-dest-center c2c-source-center 1e-8)))
      (command "_.COPY" c2c-entity-src "" c2c-source-center c2c-dest-center)
    )
    (setq c2c-idx (1+ c2c-idx))
  )

  (prompt (strcat "\nĐã copy tới " (itoa c2c-count) " tâm CIRCLE."))
  (setvar 'CMDECHO c2c-old-cmdecho)
  (command "_.UNDO" "_END")
  (princ)
)

;***************************************************************************************************************************************************************************************************************
;                                          <=============.............................. CTC ......................................... END;
;***************************************************************************************************************************************************************************************************************





;***************************************************************************************************************************************************************************************************************
;                                 START ............................... IPP-Insert-Phao Gia ..................................... =========>;
;***************************************************************************************************************************************************************************************************************

;; Hàm phụ lấy measurement từ DIM
(defun _PickDimensionAndGetMeasurement (promptMessage
                                        / pickedEntityName entityType entityObject measuredValue)
  (while (not measuredValue)
    (setq pickedEntityName (car (entsel promptMessage)))
    (cond
      ((null pickedEntityName) (prompt "\n[!] Không chọn gì."))
      ((setq entityType (cdr (assoc 0 (entget pickedEntityName))))
       (if (/= entityType "DIMENSION")
         (prompt "\n[!] Hãy chọn đúng đối tượng DIMENSION.")
         (progn
           (setq entityObject (vlax-ename->vla-object pickedEntityName))
           (setq measuredValue (vla-get-Measurement entityObject))
         )
       ))
    )
  )
  measuredValue
)

(defun C:IPP-Insert-PG ( / ent blkObj propList propObj valueWidth valueHeight wrapOnWall blockName insertPoint)

  ;; Nhập phào ôm tường (Enter = 60)
  (initget 6)
  (setq wrapOnWall (getint "\nNhập phào ôm tường <60>: "))
  (if (not wrapOnWall) (setq wrapOnWall 60))

  ;; Lấy kích thước từ DIM
  (setq valueWidth  (_PickDimensionAndGetMeasurement "\nChọn DIM chiều RỘNG: "))
  (setq valueHeight (_PickDimensionAndGetMeasurement "\nChọn DIM chiều CAO: "))

  ;; Tên block
  (setq blockName (strcat "PG-" (itoa wrapOnWall)))

  ;; Điểm chèn
  (setq insertPoint (getpoint "\nChọn điểm chèn block: "))
  (if (not insertPoint) (setq insertPoint (getvar 'lastpoint)))

  ;; Insert block
  (command "_.-INSERT" blockName insertPoint 1.0 1.0 0.0)
  (setq ent (entlast))
  (setq blkObj (vlax-ename->vla-object ent))

  ;; Lấy danh sách property
  (setq propList (vlax-invoke blkObj 'GetDynamicBlockProperties))

  ;; Duyệt qua từng property và set giá trị
  (foreach propObj propList
    (cond
      ((= (strcase (vla-get-PropertyName propObj)) "CAO")
       (vla-put-Value propObj (vlax-make-variant valueHeight 5))
      )
      ((= (strcase (vla-get-PropertyName propObj)) "RONG")
       (vla-put-Value propObj (vlax-make-variant valueWidth 5))
      )
    )
  )

  (vla-Update blkObj)
  ;(command "_.REGEN")
  (princ "\n✔ Đã chèn block và cập nhật tham số động.")
  (princ)
)

(defun C:IPS-Insert-PGS ( / ent blkObj propList propObj valueWidth valueHeight wrapOnWall blockName insertPoint)

  ;; Nhập phào ôm tường (Enter = 60)
  (initget 6)
  (setq wrapOnWall (getint "\nNhập phào ôm tường <60>: "))
  (if (not wrapOnWall) (setq wrapOnWall 60))

  ;; Lấy kích thước từ DIM
  (setq valueWidth  (_PickDimensionAndGetMeasurement "\nChọn DIM chiều RỘNG: "))
  (setq valueHeight (_PickDimensionAndGetMeasurement "\nChọn DIM chiều CAO: "))

  ;; Tên block
  (setq blockName (strcat "PGS-" (itoa wrapOnWall)))

  ;; Điểm chèn
  (setq insertPoint (getpoint "\nChọn điểm chèn block: "))
  (if (not insertPoint) (setq insertPoint (getvar 'lastpoint)))

  ;; Insert block
  (command "_.-INSERT" blockName insertPoint 1.0 1.0 0.0)
  (setq ent (entlast))
  (setq blkObj (vlax-ename->vla-object ent))

  ;; Lấy danh sách property
  (setq propList (vlax-invoke blkObj 'GetDynamicBlockProperties))

  ;; Duyệt qua từng property và set giá trị
  (foreach propObj propList
    (cond
      ((= (strcase (vla-get-PropertyName propObj)) "CAO")
       (vla-put-Value propObj (vlax-make-variant valueHeight 5))
      )
      ((= (strcase (vla-get-PropertyName propObj)) "RONG")
       (vla-put-Value propObj (vlax-make-variant valueWidth 5))
      )
    )
  )

  (vla-Update blkObj)
  ;(command "_.REGEN")
  (princ "\n✔ Đã chèn block và cập nhật tham số động.")
  (princ)
)



;***************************************************************************************************************************************************************************************************************
;                                          <=============.............................. IPP ......................................... END;
;***************************************************************************************************************************************************************************************************************


;***************************************************************************************************************************************************************************************************************
;                                 START ............................... sdd-smart-dim-o2p ..................................... =========>;
;***************************************************************************************************************************************************************************************************************

(defun get-selection-bbox (selectionSet / index entity obj
                                         minVar maxVar
                                         entityMin entityMax
                                         globalMin globalMax)
  (setq index 0)

  (while (< index (sslength selectionSet))
    (setq entity (ssname selectionSet index))
    (setq obj (vlax-ename->vla-object entity))

    (if (vlax-method-applicable-p obj 'GetBoundingBox)
      (progn
        (vla-getBoundingBox obj 'minVar 'maxVar)
        (setq entityMin (vlax-safearray->list minVar))
        (setq entityMax (vlax-safearray->list maxVar))

        (if (null globalMin)
          (progn
            (setq globalMin entityMin)
            (setq globalMax entityMax)
          )
          (progn
            (setq globalMin
              (list
                (min (car globalMin) (car entityMin))
                (min (cadr globalMin) (cadr entityMin))
                0
              )
            )
            (setq globalMax
              (list
                (max (car globalMax) (car entityMax))
                (max (cadr globalMax) (cadr entityMax))
                0
              )
            )
          )
        )
      )
    )
    (setq index (1+ index))
  )

  (list globalMin globalMax)
)

(defun c:sdd-smart-dim-o2p ( / selectionSet bbox
                    blkMin blkMax plMin plMax
                    blkXmin blkXmax blkYmin blkYmax
                    plXmin  plXmax  plYmin  plYmax
                    dimXInside dimYAbove
                    ptBlk ptPl dimPos
                    plEnt plObj )

  (vl-load-com)
  (setvar "OSMODE" 16384)
  ;; ============================
  ;; QUÉT ĐỐI TƯỢNG ĐO (BLOCK HOẶC CỤM)
  (prompt "\nQuét đối tượng cần đo (Block hoặc cụm đối tượng): ")
  (setq selectionSet (ssget))
  (if (null selectionSet) (exit))

  ;; ============================
  ;; LẤY BOUNDING BOX TỔNG
  (setq bbox (get-selection-bbox selectionSet))
  (setq blkMin (car bbox))
  (setq blkMax (cadr bbox))

  ;; ============================
  ;; CHỌN POLYLINE
  (setq plEnt (car (entsel "\nChọn POLYLINE: ")))
  (if (null plEnt) (exit))

  (setq plObj (vlax-ename->vla-object plEnt))
  (vla-getBoundingBox plObj 'plMin 'plMax)
  (setq plMin (vlax-safearray->list plMin))
  (setq plMax (vlax-safearray->list plMax))

  ;; ============================
  ;; TÁCH TỌA ĐỘ
  (setq blkXmin (car blkMin)
        blkYmin (cadr blkMin)
        blkXmax (car blkMax)
        blkYmax (cadr blkMax))

  (setq plXmin (car plMin)
        plYmin (cadr plMin)
        plXmax (car plMax)
        plYmax (cadr plMax))

  ;; ============================
  ;; THAM SỐ DIM
  (setq dimXInside (- plXmin 200))
  (setq dimYAbove  (+ plYmax 200))

  ;; ============================
  ;; DIM TRÊN
  (setq ptBlk (list blkXmin blkYmax 0))
  (setq ptPl  (list blkXmin plYmax 0))
  (setq dimPos (list dimXInside (+ plYmax 120) 0))
  (command "_.DIMLINEAR" ptBlk ptPl dimPos "")

  ;; DIM DƯỚI
  (setq ptBlk (list blkXmin blkYmin 0))
  (setq ptPl  (list blkXmin plYmin 0))
  (setq dimPos (list dimXInside (- plYmin 120) 0))
  (command "_.DIMLINEAR" ptBlk ptPl dimPos "")

  ;; DIM TRÁI
  (setq ptBlk (list blkXmin blkYmin 0))
  (setq ptPl  (list plXmin blkYmin 0))
  (setq dimPos (list (- plXmin 120) dimYAbove 0))
  (command "_.DIMLINEAR" ptBlk ptPl dimPos "")

  ;; DIM PHẢI
  (setq ptBlk (list blkXmax blkYmin 0))
  (setq ptPl  (list plXmax blkYmin 0))
  (setq dimPos (list (+ plXmax 120) dimYAbove 0))
  (command "_.DIMLINEAR" ptBlk ptPl dimPos "")
  
  (setvar "OSMODE" 16383)
  (princ "\n✔ DIM xong – tự nhận dạng Block / Cụm đối tượng.")
  (princ)
)



;***************************************************************************************************************************************************************************************************************
;                                          <=============.............................. sdd-smart-dim-o2p ......................................... END;
;***************************************************************************************************************************************************************************************************************


(defun c:sdd-smart-dim-o2p ( / *error* oldos selectionSet plSet bbox
                    blkMin blkMax plMin plMax
                    blkXmin blkXmax blkYmin blkYmax
                    plXmin  plXmax  plYmin  plYmax
                    dimXInside dimYAbove
                    ptBlk ptPl dimPos
                    idx plEnt plObj )

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
  (setvar "OSMODE" 16384)

  (prompt "\nQuet doi tuong can do (Block hoac cum doi tuong): ")
  (setq selectionSet (ssget))
  (if (null selectionSet)
    (progn
      (setvar "OSMODE" oldos)
      (exit)
    )
  )

  (setq bbox (get-selection-bbox selectionSet))
  (setq blkMin (car bbox))
  (setq blkMax (cadr bbox))

  (setq blkXmin (car blkMin)
        blkYmin (cadr blkMin)
        blkXmax (car blkMax)
        blkYmax (cadr blkMax))

  (prompt "\nQuet 1 hoac nhieu POLYLINE: ")
  (setq plSet (ssget '((0 . "LWPOLYLINE,POLYLINE"))))
  (if (null plSet)
    (progn
      (setvar "OSMODE" oldos)
      (exit)
    )
  )

  (setq idx 0)
  (while (< idx (sslength plSet))
    (setq plEnt (ssname plSet idx))
    (setq plObj (vlax-ename->vla-object plEnt))
    (vla-getBoundingBox plObj 'plMin 'plMax)
    (setq plMin (vlax-safearray->list plMin))
    (setq plMax (vlax-safearray->list plMax))

    (setq plXmin (car plMin)
          plYmin (cadr plMin)
          plXmax (car plMax)
          plYmax (cadr plMax))

    (setq dimXInside (- plXmin 200))
    (setq dimYAbove  (+ plYmax 200))

    (setq ptBlk (list blkXmin blkYmax 0))
    (setq ptPl  (list blkXmin plYmax 0))
    (setq dimPos (list dimXInside (+ plYmax 120) 0))
    (command "_.DIMLINEAR" ptBlk ptPl dimPos "")

    (setq ptBlk (list blkXmin blkYmin 0))
    (setq ptPl  (list blkXmin plYmin 0))
    (setq dimPos (list dimXInside (- plYmin 120) 0))
    (command "_.DIMLINEAR" ptBlk ptPl dimPos "")

    (setq ptBlk (list blkXmin blkYmin 0))
    (setq ptPl  (list plXmin blkYmin 0))
    (setq dimPos (list (- plXmin 120) dimYAbove 0))
    (command "_.DIMLINEAR" ptBlk ptPl dimPos "")

    (setq ptBlk (list blkXmax blkYmin 0))
    (setq ptPl  (list plXmax blkYmin 0))
    (setq dimPos (list (+ plXmax 120) dimYAbove 0))
    (command "_.DIMLINEAR" ptBlk ptPl dimPos "")

    (setq idx (1+ idx))
  )

  (setvar "OSMODE" oldos)
  (princ "\nDim xong cho cac POLYLINE da quet.")
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:UM-UNDO-MARK ()
  (command "_.undo" "_mark")
  (princ)
)

(defun c:UB-UNDO-BACK ()
  (command "_.undo" "_back")
  (princ)
)

(defun dx-layer-set-on (lay onFlag /)
  (if (tblsearch "LAYER" lay)
    (progn
      (vl-load-com)
      (vla-put-LayerOn
        (vla-Item
          (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object)))
          lay
        )
        (if onFlag :vlax-true :vlax-false)
      )
      T
    )
    nil
  )
)

(defun cccat-cleanup ( / )
  (if *cccat-reactor*
    (progn
      (vlr-remove *cccat-reactor*)
      (setq *cccat-reactor* nil)
    )
  )
  (if *cccat-layer*
    (dx-layer-set-on *cccat-layer* T)
  )
  (if (and *cccat-old-layer*
           (tblsearch "LAYER" *cccat-old-layer*))
    (setvar "CLAYER" *cccat-old-layer*)
  )
  (setq *cccat-layer* nil)
  (setq *cccat-old-layer* nil)
)

(defun cccat-command-ended (reactor params / cmd)
  (setq cmd (car params))
  (if (wcmatch (strcase cmd) "*CCC_SMART_COPY_TO_CENTER*")
    (cccat-cleanup)
  )
)

(defun cccat-command-cancelled (reactor params / cmd)
  (setq cmd (car params))
  (if (wcmatch (strcase cmd) "*CCC_SMART_COPY_TO_CENTER*")
    (cccat-cleanup)
  )
)

(defun c:C3-COPY-KINH ( / *error* )
  (vl-load-com)

  (defun *error* (msg)
    (cccat-cleanup)
    (if (and msg
             (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
      (prompt (strcat "\nLoi: " msg))
    )
    (princ)
  )

  (cccat-cleanup)
  (setq *cccat-layer* "_mss.cat")
  (setq *cccat-old-layer* (getvar "CLAYER"))

  (if (and (= (strcase *cccat-old-layer*) (strcase *cccat-layer*))
           (tblsearch "LAYER" "0"))
    (setvar "CLAYER" "0")
  )

  (dx-layer-set-on *cccat-layer* nil)

  (setq *cccat-reactor*
    (vlr-command-reactor
      nil
      '(
        (:vlr-commandEnded . cccat-command-ended)
        (:vlr-commandCancelled . cccat-command-cancelled)
        (:vlr-commandFailed . cccat-command-cancelled)
      )
    )
  )

  (command "CCC_SMART_COPY_TO_CENTER")
  (princ)
)

;;;;;;;;;;;;;;;; END OF CCC




(defun dungx-eq (a b /)
  (< (abs (- a b)) 1e-8)
)

(defun dungx-dot2d (v1 v2 /)
  (+ (* (car v1) (car v2))
     (* (cadr v1) (cadr v2)))
)

(defun dungx-cross2d (v1 v2 /)
  (- (* (car v1) (cadr v2))
     (* (cadr v1) (car v2)))
)

(defun dungx-vsub2d (p1 p2 /)
  (list (- (car p1) (car p2))
        (- (cadr p1) (cadr p2))
        0.0)
)

(defun dungx-zero-vec-p (v /)
  (and (dungx-eq (car v) 0.0)
       (dungx-eq (cadr v) 0.0))
)

(defun dungx-collinear-pt-p (pPrev pCur pNext / v1 v2)
  (setq v1 (dungx-vsub2d pCur pPrev))
  (setq v2 (dungx-vsub2d pNext pCur))
  (dungx-eq (dungx-cross2d v1 v2) 0.0)
)

(defun dungx-simplify-rect-pts (pts / changed res i pPrev pCur pNext)
  (setq changed T)
  (while changed
    (setq changed nil)
    (setq res nil)
    (setq i 0)
    (while (< i (length pts))
      (setq pPrev (nth (rem (+ i (1- (length pts))) (length pts)) pts))
      (setq pCur  (nth i pts))
      (setq pNext (nth (rem (1+ i) (length pts)) pts))
      (if (and (> (length pts) 4)
               (dungx-collinear-pt-p pPrev pCur pNext))
        (setq changed T)
        (setq res (append res (list pCur)))
      )
      (setq i (1+ i))
    )
    (setq pts res)
  )
  pts
)

(defun dungx-rect-frame-p (ent / obj coords pts i pt e1 e2 e3 e4)
  (and ent
       (= "LWPOLYLINE" (cdr (assoc 0 (entget ent))))
       (setq obj (vlax-ename->vla-object ent))
       (setq coords (vlax-get obj 'Coordinates))
       (>= (length coords) 8)
       (= 0 (rem (length coords) 2))
       (progn
         (setq pts nil
               i 0)

         (while (< i (/ (length coords) 2))
            (setq pt (list (nth (* i 2) coords) (nth (1+ (* i 2)) coords) 0.0))
            (setq pts (append pts (list pt)))
            (if (not (dungx-eq (vla-GetBulge obj i) 0.0))
              (setq pts nil i (/ (length coords) 2))
            )
            (setq i (1+ i))
          )

         (if pts
           (progn
             (setq pts (dungx-simplify-rect-pts pts))
             (if (= (length pts) 4)
               (progn
                 (setq e1 (dungx-vsub2d (nth 1 pts) (nth 0 pts)))
                 (setq e2 (dungx-vsub2d (nth 2 pts) (nth 1 pts)))
                 (setq e3 (dungx-vsub2d (nth 3 pts) (nth 2 pts)))
                 (setq e4 (dungx-vsub2d (nth 0 pts) (nth 3 pts)))
                 (and
                   (not (dungx-zero-vec-p e1))
                   (not (dungx-zero-vec-p e2))
                   (not (dungx-zero-vec-p e3))
                   (not (dungx-zero-vec-p e4))
                   (dungx-eq (dungx-dot2d e1 e2) 0.0)
                   (dungx-eq (dungx-dot2d e2 e3) 0.0)
                   (dungx-eq (dungx-cross2d e1 e3) 0.0)
                   (dungx-eq (dungx-cross2d e2 e4) 0.0)
                 )
               )
               nil
             )
           )
           nil
         )
       )
  )
)

(defun dungx-filter-rect-frames (ss / out idx ent)
  (setq idx 0)
  (while (< idx (sslength ss))
    (setq ent (ssname ss idx))
    (if (dungx-rect-frame-p ent)
      (progn
        (if (null out)
          (setq out (ssadd))
        )
        (ssadd ent out)
      )
    )
    (setq idx (1+ idx))
  )
  out
)

(defun dungx-filter-frame-like-plines (ss / out idx ent obj minpt maxpt pmin pmax)
  (setq idx 0)
  (while (< idx (sslength ss))
    (setq ent (ssname ss idx))
    (setq obj (vlax-ename->vla-object ent))
    (vla-getBoundingBox obj 'minpt 'maxpt)
    (setq pmin (vlax-safearray->list minpt))
    (setq pmax (vlax-safearray->list maxpt))
    (if (and (> (abs (- (car pmax) (car pmin))) 1e-8)
             (> (abs (- (cadr pmax) (cadr pmin))) 1e-8))
      (progn
        (if (null out)
          (setq out (ssadd))
        )
        (ssadd ent out)
      )
    )
    (setq idx (1+ idx))
  )
  out
)

(defun c:DUNGX (/ selection-set frame-list frame
                  entity max-point
                  insert-point date-point number-point
                  current-time date-string year month day formatted-date
                  offset-x offset-y counter)

  (prompt "\n🚀 DUNGX: Auto insert SIGNATURE theo khung in...")

  ;; ================= CONFIG =================
  (setq offset-x 330)  ;; thụt trái
  (setq offset-y 100)  ;; thụt xuống
  ;; ==========================================

  (setvar "OSMODE" 0)
  (setvar "CLAYER" "_mss.phantom")

  ;; Check block SIGNATURE
  (if (not (tblsearch "BLOCK" "SIGNATURE"))
    (progn
      (princ "\n❌ Không tìm thấy block SIGNATURE.")
      (exit)
    )
  )

  ;; Get frame polylines
  (setq selection-set
        (ssget "X"
          '(
            (0 . "LWPOLYLINE")
            (8 . "_mss.in")
          )
        )
  )
  (if selection-set
    (setq selection-set (dungx-filter-frame-like-plines selection-set))
  )

  (if (not selection-set)
    (progn
      (princ "\n⚠️ Không tìm thấy frame polyline hợp lệ trên layer _mss.in.")
      (exit)
    )
  )

  ;; Sort frames
  (setq frame-list (sort-frames-left-to-right selection-set))


  ;; Get system date
  (setq current-time (getvar "CDATE"))
  (setq date-string (substr (rtos current-time 2 6) 1 8))
  (setq year  (substr date-string 1 4))
  (setq month (substr date-string 5 2))
  (setq day   (substr date-string 7 2))
  (setq formatted-date (strcat day "/" month "/" year))

  (setq counter 1)

  ;; Insert loop
  (foreach frame frame-list
    (setq entity (cdr frame))


    (vla-getBoundingBox
      (vlax-ename->vla-object entity)
      'min-point
      'max-point
    )

    (setq max-point (vlax-safearray->list max-point))

    ;; Insert point (top-right, offset inward)
    (setq insert-point
          (list
            (- (car max-point) offset-x)
            (- (cadr max-point) offset-y)
            0
          )
    )

    ;; Insert block
    (command "_.-INSERT" "SIGNATURE" insert-point "" "")

    ;; Text positions
    (setq date-point
          (list
            (- (car insert-point) 200)
            (- (cadr insert-point) 150)
            0
          )
    )

    (setq number-point
          (list
            (- (car insert-point) 100)
            (- (cadr insert-point) 250)
            0
          )
    )

    ;; Create texts
    (create-text date-point formatted-date 55 "_mss.phantom")
    (create-text number-point (strcat "NO." (itoa counter)) 55 "_mss.phantom")

    (setq counter (1+ counter))
  )

  (setvar "OSMODE" 16383)
  (setvar "CLAYER" "_mss.kichthuoc")

  (prompt
    (strcat
      "\n✅ HOÀN THÀNH: "
      (itoa (1- counter))
      " khung đã được đánh số (TRÁI → PHẢI)."
    )
  )
  (princ)
)
