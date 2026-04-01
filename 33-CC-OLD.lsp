

(defun m33-mirror-cleanup ( )
  (if *m33-mirror-reactor*
    (progn
      (vlr-remove *m33-mirror-reactor*)
      (setq *m33-mirror-reactor* nil)
    )
  )
  (if (not (null *m33-pickfirst-old*))
    (progn
      (setvar "PICKFIRST" *m33-pickfirst-old*)
      (setq *m33-pickfirst-old* nil)
    )
  )
  (if (not (null *m33-osmode-old*))
    (progn
      (setvar "OSMODE" *m33-osmode-old*)
      (setq *m33-osmode-old* nil)
    )
  )
  (setq *m33-entlast* nil)
)

(defun m33-mirror-finish ( / ssNew )
  (setq ssNew (cc-ssnewer *m33-entlast*))
  (setq p ssNew)

  (if p
    (progn
      (sssetfirst nil p)
      (prompt
        (strcat
          "\nDa luu "
          (itoa (sslength p))
          " doi tuong moi vao bien P. Go !P de dung lai."
        )
      )
    )
    (prompt "\nKhong co doi tuong moi duoc tao.")
  )

  (m33-mirror-cleanup)
)

(defun m33-mirror-ended (reactor params)
  (if (wcmatch (strcase (car params)) "*MIRROR")
    (m33-mirror-finish)
  )
)

(defun m33-mirror-cancelled (reactor params)
  (if (wcmatch (strcase (car params)) "*MIRROR")
    (m33-mirror-cleanup)
  )
)

(defun c:33 ( )
  (vl-load-com)
  (m33-mirror-cleanup)
  (setq *m33-entlast* (entlast))
  (setq *m33-pickfirst-old* (getvar "PICKFIRST"))
  (setq *m33-osmode-old* (getvar "OSMODE"))
  (setvar "PICKFIRST" 1)
  (setvar "OSMODE" 16383)

  (setq *m33-mirror-reactor*
    (vlr-command-reactor
      nil
      '(
        (:vlr-commandEnded . m33-mirror-ended)
        (:vlr-commandCancelled . m33-mirror-cancelled)
        (:vlr-commandFailed . m33-mirror-cancelled)
      )
    )
  )

  (if *m33-mirror-reactor*
    (progn
      (prompt "\nSelect objects: quet binh thuong hoac go !P de dung bien P.")
      (command "_.MIRROR")
    )
  )
  (princ)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;lệnh copy -cc không lặp lại nhiều lần
(defun cc-ssnewer (ent / ss e)
  (setq e (if ent (entnext ent) (entnext)))
  (while e
    (if (null ss) (setq ss (ssadd)))
    (setq ss (ssadd e ss))
    (setq e (entnext e))
  )
  ss
)

(defun cc-copy-cleanup ( )
  (if *cc-copy-reactor*
    (progn
      (vlr-remove *cc-copy-reactor*)
      (setq *cc-copy-reactor* nil)
    )
  )
  (if (not (null *cc-pickfirst-old*))
    (progn
      (setvar "PICKFIRST" *cc-pickfirst-old*)
      (setq *cc-pickfirst-old* nil)
    )
  )
  (setq *cc-entlast* nil)
)

(defun cc-copy-finish ( / ssNew )
  (setq ssNew (cc-ssnewer *cc-entlast*))
  (setq p ssNew)

  (if p
    (progn
      (sssetfirst nil p)
      (prompt
        (strcat
          "\nDa luu "
          (itoa (sslength p))
          " doi tuong moi vao bien P. Go !P de dung lai."
        )
      )
    )
    (prompt "\nKhong co doi tuong moi duoc tao.")
  )

  (cc-copy-cleanup)
)

(defun cc-copy-ended (reactor params)
  (if (wcmatch (strcase (car params)) "*COPY")
    (cc-copy-finish)
  )
)

(defun cc-copy-cancelled (reactor params)
  (if (wcmatch (strcase (car params)) "*COPY")
    (cc-copy-cleanup)
  )
)

(defun c:CC ( )
  (vl-load-com)
  (cc-copy-cleanup)
  (setq *cc-entlast* (entlast))
  (setq *cc-pickfirst-old* (getvar "PICKFIRST"))
  (setvar "PICKFIRST" 1)

  (setq *cc-copy-reactor*
    (vlr-command-reactor
      nil
      '(
        (:vlr-commandEnded . cc-copy-ended)
        (:vlr-commandCancelled . cc-copy-cancelled)
        (:vlr-commandFailed . cc-copy-cancelled)
      )
    )
  )

  (if *cc-copy-reactor*
    (progn
      (prompt "\nSelect objects: quet binh thuong hoac go !P de dung bien P.")
      (command "_.COPY")
    )
  )
  (princ)
)