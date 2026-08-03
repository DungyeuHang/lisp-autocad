       
; ---------------- CHUONG TRINH 1 - KHOI TAO ----------------------;
(defun khoitao ()

  ;;Cac goc hay dung
  (setq g0   0.0
        g45  (* 0.25 pi)
	g90  (* 2.0 g45)
	g135 (* 3.0 g45)
	g180 (* 4.0 g45)
	g225 (* 5.0 g45)
	g270 (* 6.0 g45)
	g315 (* 7.0 g45)
        g123  (* pi (/ 123 180))
	g140  (* pi (/ 140 180))
	g125  (* pi (/ 125 180))
	g241  (* pi (/ 241 180))

   )

  
  ;; Cac che do Osmode, orthomode... luu gia tri hien hanh va dat ve 0
  (setq	osmode	  (getvar "osmode")
	orthomode (getvar "orthomode")
	lts       (getvar "ltscale")
  )
  (setvar "osmode" 0)
  (setvar "orthomode" 0)
  (command-s "ltscale" 1.0)
    
  ;; Khoi tao cac lop (layer) hay dung
  (setq clay (getvar "clayer"))
  (command-s "-layer" "m"	"_mss.bao" "c" 7 "" "lt" "Continuous" "" "")
  (command-s "-layer" "m"	"_mss.dut" "c" 1 "" "lt" "hidden" "" "")
  (command-s "-layer" "m"	"_mss.tam" "c" 1 "" "lt" "center" "" "")
  (command-s "-layer" "m"	"_mss.kichthuoc" "c" 2 "" "lt" "Continuous" "" "")
  (command-s "-layer" "m"	"_mss.cat" "c" 8 "" "lt" "Continuous" "" "")
  (command-s "-layer" "m"	"_mss.phantom" "c" 2 ""	"lt" "Continuous" "" "")
  (command-s "-layer" "m"	"_mss.pro" "c" 6 "" "lt" "Continuous" "" "")
  (command-s "-layer" "m"	"_mss.text" "c"	2 "" "lt" "Continuous" "" "")
  (command-s "-layer" "m"	"_mss.canh" "c"	3 "" "lt" "Continuous" "" "")
  (setvar "clayer" clay)
  
)
;------------------- KET THUC --------------------------------------
(defun ketthuc ()
  ;; Bat Cac che do Osmode, orthomode... 
  (setq	osmode	  (getvar "osmode")
	orthomode (getvar "orthomode")
	lts       (getvar "ltscale")
  )
  (setvar "osmode" 16383) 
  (command-s "ltscale" 5.0)
    
)


(defun dulieuluu ()     
  ;Xu li du lieu cua hop thoai
    (setq keys '("le1" "mdh" "mc" "mh1" "mh2" "mtk" "r" "c" "bt" "bk" "ctt" "ld" "bot" "ds1" "mm1" "hm1" "ccm" "cot1" "kh1" "pg1" 
                  "rk1" "lk1" "lk2" "nv1"))
  (setq file (open "C:\\Users\\Admin\\Desktop\\DULIEU.txt" "w")) 
  (foreach key keys
    (setq val (get_tile key))
    (write-line (strcat key "=" val) file)  ;Gán thông số hộp thoại vào các biến
  )
  (close file)
)

; ---------------- CHUONG TRINH 2 - HOP THOAI ----------------------;
         
(defun hopthoai ()

  ;; Nap file DCL v�o bo nho
  (setq dcl_id (load_dialog "BangThongSoCuaKMD.dcl"))
     (if (not (new_dialog "hopthoai1" dcl_id))
       (progn
         (alert "DCL CHUA DINH NGHIA")
         (exit)
       )
    (if huongphaomacdinh
    (set_tile "hp1" huongphaomacdinh))
  )
	(setq ng1 (GETCURRENTDATE))
	(set_tile "ng1" ng1)

  (upDateImage "hinhcua.sld" "hinhcua_image")

   ;; Đọc dữ liệu từ file DULIEU.txt nếu có
  (if (findfile "C:\\Users\\Admin\\Desktop\\DULIEU.txt")
    (progn
      (setq file (open "C:\\Users\\Admin\\Desktop\\DULIEU.txt" "r"))
      (setq data '())
      (while (setq line (read-line file))
        (if (vl-string-search "=" line)
          (progn
            (setq key (vl-string-trim " " (substr line 1 (vl-string-search "=" line))))
            (setq val (vl-string-trim " " (substr line (+ 2 (vl-string-search "=" line)))))
            (setq data (cons (list key val) data))
          )
        )
      )
      (close file)

      ;; Gán giá trị vào các tile nếu có key tương ứng
      (foreach pair data
        (if (get_tile (car pair))
          (set_tile (car pair) (cadr pair))
        )
      )
    )
  )
  
  ;; Cai dat khi nguoi dung nhan nut "Xong" hoac "huy"
  (action_tile "accept" "(setq ddiag 2)(dulieu)(dulieuluu)(done_dialog)")    ; o day da su dung h�m du lieu roi do do chuong trinh chinh k can khai bao su dung
  (action_tile "cancel" "(setq ddiag 1)(done_dialog) (exit)")
  ;;(princ "ok")
  (start_dialog)
  (unload_dialog dcl_id)

)     
(defun upDateImage(sldName hinhcua_image) 
	;;;--- Get the width of the slide  
	(setq width (dimx_tile hinhcua_image))
	;;;--- Get the height of the slide 
	(setq height (dimy_tile hinhcua_image)) 
	;;;--- Start the slide definition
	(start_image hinhcua_image)
	;;;--- Wipe out the background 
	;; (fill_image 0 0 width height -16)
	;;;--- Put the slide in the image area  
	(slide_image 0 0 width height sldName)
	;;;--- Finish the slide definition 
	(end_image)
)

; ---------------- CHUONG TRINH 3 - DU LIEU ----------------------;

(defun dulieu ()          ;Xu li du lieu cua hop thoai
                                            ;G�n cac thong so cua hop thoai vao cac bien
   (setq r   (atof (get_tile "r")))            
   (setq c   (atof (get_tile "c")))           
   (setq bk  (atof (get_tile "bk")))
   (setq bot (atof (get_tile "bot")))
   (setq bt  (atof (get_tile "bt")))
   (setq ctt (atof (get_tile "ctt")))
   (setq ds1 (atof (get_tile "ds1")))
   (setq ld (atof (get_tile "ld")))
   (setq lk1 (atof (get_tile "lk1")))
   (setq rk1 (atof (get_tile "rk1")))
   (setq lk2 (atof (get_tile "lk2")))
   (setq mdh  (get_tile "mdh"))
   (setq ccm  (get_tile "ccm"))
   (setq msp  (get_tile "msp"))
   (setq mc   (get_tile "mc"))
   (setq mh1  (get_tile "mh1"))
   (setq mh2  (get_tile "mh2"))
   (setq mtk  (get_tile "mtk"))
   (setq cot1 (get_tile "cot1"))
   (setq le1  (get_tile "le1"))
   (setq mm1  (get_tile "mm1"))
   (setq hp1  (get_tile "hp1"))
   (setq hm1  (get_tile "hm1"))
   (setq kh1  (get_tile "kh1"))
   (setq pg1  (get_tile "pg1"))
   (setq nv1  (get_tile "nv1"))
   (setq ng1  (get_tile "ng1"))
  
  (if (< bk 200);;;;;;;;;;NHẬP RỘNG THÔNG THUỶ NẾU CÓ
  (setq bk bk) ; giữ nguyên
  (setq bk (/ (- (- r 5) bk) 2)) ; tính lại
  )
  
  (setq slbthm (vl-string-search "-" hm1));;;;;;;;;;;;;;;;;;tách số lượng bộ để ghi chú số lượng cho các chi tiết
  (setq slb (if slbthm (atoi (substr hm1 1 slbthm)) (atoi hm1)))

)


; ----------------  TRIEN KHAI BAN VE ----------------------;
;;PHÀO GIẢ TIÊU CHUẨN ÔM TƯỜNG 50 TRỞ LÊN
(defun PHAOGIATIEUCHUAN50 ()
  
(setq Camsau (getreal "\n camsautuongbaonhieu:"));;;;HỎI PHẦN CẤM SÂU TRƯỚC KHI CHẠY
  (if (null Camsau)
    (setq Camsau 35.0)
  )
  
  (if (< (- bot 30) 20);;;;;;;;;;KIỂM TRA NẾU BOT NHỎ HƠN 15 THÌ KHÔNG CHẠY NỮA
    (progn
      ;; Hiện bảng thông báo lỗi
      (alert "KHÔNG CHẤN ĐƯỢC ĐÂU ÔM TƯỜNG PHẢI 50 TRỞ LÊN!\nTĂNG PHÀO ÔM TƯỜNG LÊN HOẶC DÙNG BIÊN DẠNG KHÁC.")
      ;; Hủy lệnh và dừng toàn bộ chương trình ngay lập tức
      (exit) 
    )
  )
  
  ; ---------------- TAO LAP CAC DIEM ---------------------;-
(setq
  ; 1- PHÀO GIẢ NGANG
  PG1  (getpoint "\n Chon diem bat dau:")
	PG2  (polar PG1 g90 (- Camsau 1))
	PG3  (polar PG2 g0 1.1)
	PG4  (polar (polar PG3 g90 14) g0 14)
	PG5  (polar (polar PG4 g90 21.2) g0 15)
	PG6  (polar (polar PG5 g90 (- (- bot 30) 1.4)) g0 (- (- bot 30) 1.4))
	PG7  (polar PG6 g90 13)
	PG8  (polar (polar PG7 g90 8.8) g180 8.8)
	PG9   (polar PG1 g180 (- r 2))
	PG10  (polar PG9 g90 (- Camsau 1))
	PG11  (polar PG10 g180 1.1)
	PG12  (polar (polar PG11 g90 14) g180 14)
	PG13  (polar (polar PG12 g90 21.2) g180 15)
	PG14  (polar (polar PG13 g90 (- (- bot 30) 1.4)) g180 (- (- bot 30) 1.4))
	PG15  (polar PG14 g90 13)
	PG16  (polar (polar PG15 g90 8.8) g0 8.8)
	
; 2- PHÀO GIẢ ĐỨNG
  PG17  (polar PG1 g90 600)
	PG18  (polar PG17 g90 (- Camsau 1))
	PG19  (polar PG18 g0 1.1)
	PG20  (polar (polar PG19 g90 14) g0 14)
	PG21  (polar (polar PG20 g90 21.2) g0 15)
	PG22  (polar (polar PG21 g90 (- (- bot 30) 1.4)) g0 (- (- bot 30) 1.4))
	PG23  (polar PG22 g90 13)
	PG24  (polar (polar PG23 g90 8.8) g180 8.8)
	PG25  (polar PG17 g180 (- c 2))
  PG26  (polar PG25 g90 (- Camsau 1))
  PG27  (polar PG26 g90 14)
  PG28  (polar PG27 g90 21.2)
  PG29  (polar PG28 g90 (- (- bot 30) 1.4))
  PG30  (polar PG29 g90 13)
	PG31  (polar PG30 g90 8.8)
  
; 3- HÌNH CHIẾU CHẤN
	HCC1 (polar PG1 g0 700)
	HCC2 (polar HCC1 g0 (* Camsau 4))
	HCC3 (polar HCC2 g90 (* 15 4))
	HCC4 (polar (polar HCC3 g90 (* 15 4)) g0 (* 15 4))
	HCC5 (polar HCC4 g90 (* (- bot 30) 4))
	HCC6 (polar HCC5 g180 (* 15 4))
	HCC7 (polar HCC6 g270 (* 10 4))

  )

  ; ---------------- NOI BAN VE ----------------------;
  
; 1- PHÀO GIẢ NGANG
  (command-s "-layer" "m"	"_mss.bao" "c" 7 "" "lt" "Continuous" "" "")
  (command-s "text" "style" "VNTIME" (polar (polar PG1 g90 5) g180 150) 5 "0" (princ mdh));;;;GHI CHÚ TÊN 
  (command-s "text" "style" "VNTIME" (polar PG1 g0 150) 60 "0" (strcat "SL:" (itoa (* 1 slb)))) ;;;SỐ LƯỢNG THANH
  (command-s ".circle" (polar (polar pg1 g90 8) g180 10) 4"")
  (command-s ".circle" (polar (polar pg9 g90 8) g0 10) 4"")
  (setq pg1x1 (polar PG1 g180 5.0)) 
  (setq pg1x (polar PG1 g90 5.0))  
  (setq pg9x1 (polar PG9 g0 5.0)) 
  (setq pg9x (polar PG9 g90 5.0)) 
  
  ;; 2. VẼ POLYLINE KẾT HỢP CUNG TRÒN TỰ ĐỘNG
  (command-s "_.PLINE" PG2 PG3 PG4 PG5 PG6 PG7 PG8 PG16 PG15 PG14 PG13 PG12 PG11 PG10 pg9x "_A" pg9x1 "_L" pg1x1 "_A" pg1x "_L" "c")
  (command-s "-layer" "m"	"_mss.dut" "c" 1 "" "lt" "hidden" "" "")
  (command-s "_.PLINE" PG2 PG10"")
  (command-s "_.PLINE" PG4 PG12"")
  (command-s "_.PLINE" PG5 PG13"")
  (command-s "_.PLINE" PG6 PG14"")
  (command-s "_.PLINE" PG7 PG15"")
 ; 1- PHÀO GIẢ ĐỨNG
  (command-s "-layer" "m"	"_mss.bao" "c" 7 "" "lt" "Continuous" "" "")
  (command-s "text" "style" "VNTIME" (polar (polar PG17 g90 5) g180 150) 5 "0" (princ mdh));;;;GHI CHÚ TÊN 
  (command-s "text" "style" "VNTIME" (polar PG17 g0 150) 60 "0" (strcat "SL:" (itoa (* 2 slb)))) ;;;SỐ LƯỢNG THANH
  (command-s "text" "style" "VNTIME" (polar (polar PG17 g90 80) g0 150) 60 "0" (princ "CHAN DOI XUNG"));;;;GHI CHÚ CHẤN 
  (command-s ".circle" (polar (polar pg17 g90 8) g180 10) 4"")
  (command-s ".circle" (polar (polar pg25 g90 8) g0 10) 4"")
  (setq pg17x1 (polar PG17 g180 5.0)) 
  (setq pg17x (polar PG17 g90 5.0))  
  (setq pg25x1 (polar PG25 g0 5.0)) 
  (setq pg25x (polar PG25 g90 5.0))
  (command-s "_.PLINE" PG18 PG19 PG20 PG21 PG22 PG23 PG24 PG31 pg25x "_A" pg25x1 "_L" pg17x1 "_A" pg17x "_L" "c")
  (command-s "-layer" "m"	"_mss.dut" "c" 1 "" "lt" "hidden" "" "")
  (command-s "_.PLINE" PG18 pg26"")
  (command-s "_.PLINE" PG20 pg27"")
  (command-s "_.PLINE" PG21 pg28"")
  (command-s "_.PLINE" PG22 PG29"")
  (command-s "_.PLINE" PG23 PG30"")
  ; 8- HINH CHIEU CHAN KHUNG
  (command-s "-layer" "m"	"_mss.phantom" "c" 7 "" "lt" "Continuous" "" "")
  (command-s "_.PLINE" HCC1 HCC2 HCC3 HCC4 HCC5 HCC6 HCC7"")
  (setq nHCC (entlast))
  (command-s ".offset" 4.8 nHCC (polar HCC1 g90 4)"")
  
 ; ------------------ DIM KICH THUOC -------------------;
  ; 1- PHÀO GIẢ NGANG
  (command-s "-layer" "m"	"_mss.kichthuoc" "c" 5 "" "lt" "Continuous" "" "")
  (command-s "dimlinear" PG1 PG9 (polar (polar PG1 g180 (/ r 2)) g270 100)"")
  (command-s "dimlinear" PG9 PG16 (polar PG13 g180 150)"")
  (command-s "dimlinear" PG7 PG15 (polar (polar PG7 g90 100) g180 (/ r 2))"")
  
  ; 2- PHÀO GIẢ ĐỨNG
   (command-s "-layer" "m"	"_mss.kichthuoc" "c" 5 "" "lt" "Continuous" "" "")
  (command-s "dimlinear" PG17 PG25 (polar (polar PG17 g180 (/ r 2)) g270 100)"")
  (command-s "dimlinear" PG25 PG31 (polar PG28 g180 150)"")
  (command-s "dimlinear" PG31 PG23 (polar (polar PG23 g90 100) g180 (/ c 2))"")

  ; 3- HINH CHIEU CHAN KHUNG
  (command-s "dimlfac" 0.25"")
  (command-s "dimlinear" HCC1 HCC2 (polar (Polar HCC1 g270 120) g0 (* Camsau 2))"")
  (command-s "dimlinear" HCC2 HCC3 (polar (Polar HCC2 g90 30) g0 120)"")
  (command-s "dimaligned"  HCC3 HCC4 (polar (Polar HCC3 g45 42) g0 300)"")
  ;(command "_.DIMANGULAR" "" HCC3 HCC2 HCC4 (polar (Polar HCC3 g0 300) g270 150) "")
  (command-s "dimlinear" HCC4 HCC5 (polar (Polar HCC4 g90 (* bot 4)) g0 200)"")
  (command-s "dimlinear" HCC5 HCC6 (polar (Polar HCC5 g180 30) g90 100)"")
  (command-s "dimlinear" HCC6 HCC7 (polar (Polar HCC6 g180 100) g270 20)"")
  (command-s "dimlinear" HCC6 HCC1 (polar (Polar HCC1 g90 (/ bot 2)) g180 150)"")
  (command-s "dimlfac" 1"")
)

; ---------------- LENH CHAY BAN VE  ----------------------;
;----------01------------
(defun c:PPP1 ()
   (khoitao)
   (hopthoai)
   (setq t-start (getvar "DATE")) ; thời điểm người bắt đầu tương tác
   (PHAOGIATIEUCHUAN50)
   (ketthuc)
   (setq t-end (getvar "DATE"))
   (setq elapsed (* (- t-end t-start) 86400.0)) ; đổi sang GIAY
   (alert (strcat "TONG THOI GIAN VE: " (rtos elapsed 2 2) " GIAY"))
  )


