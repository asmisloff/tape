(defun c:make-tape  (/ offset get-input original original-layer tape-layer copy input)
  (defun offset	 (dist)
    (command "_solidedit" "_face" "_offset" pause "" dist "" ""))

  (defun get-input  (/ input)
    (setq input (getstring "\Specify thickness or [0.4/2/Exit] <0.4>: "))
    (if	(= input "")
      (setq input "0.4"))
    input)

  (setq original (car (entsel)))
  (setq original-layer (vla-get-layer (vlax-ename->vla-object original)))
  (setq input (get-input))

  (while (not (eq input "e"))
    (if	(not (= input "0"))
      (progn
	(setq tape-layer (original-layer->tape-layer original-layer input))
	(setq copy (duplicate original))
	(hide copy)
	(offset (- (abs (atof input))))

	(setq offseted-copy (duplicate original))
	(sub copy offseted-copy)
	(show (entlast))
	(change-layer tape-layer (entlast))
	(setq input (get-input))))))

					;-----------------------------------------------------------------

(defun ename  (ent)
  (if (listp ent)
    (car ent)
    ent))

(defun duplicate  (ent)
  (vla-copy (vlax-ename->vla-object (ename ent)))
  (entlast))

(defun hide  (ent)
  (vlax-put-property
    (vlax-ename->vla-object (ename ent))
    'visible
    0))

(defun show  (ent)
  (vlax-put-property
    (vlax-ename->vla-object (ename ent))
    'visible
    1))

(defun sub  (solid-1 solid-2)
  (vla-boolean
    (vlax-ename->vla-object (ename solid-1))
    acsubtraction
    (vlax-ename->vla-object (ename solid-2))))

(defun change-layer  (layer-name ent)
  (print layer-name)
  (vlax-put-property
    (vlax-ename->vla-object ent)
    'layer
    layer-name))

(defun delete  (ent)
  (vla-delete (vlax-ename->vla-object (ename original))))

(defun original-layer->tape-layer  (layer thickness / tape-layer pos color)
  (setq tape-layer (vl-string-subst "Êğîìêà" "ËÄÑÏ" layer))
  (setq tape-layer (vl-string-subst (strcat thickness "x19") "16 ìì" tape-layer))
  (setq tape-layer (vl-string-subst (strcat thickness "x28") "25 ìì" tape-layer))
  (setq tape-layer (vl-string-subst (strcat thickness "x19") "8 ìì" tape-layer))
  (setq	color (itoa (vla-get-color
		      (vla-item
			(vla-get-layers
			  (vla-get-activedocument (vlax-get-acad-object)))
			layer))))
  (command "-layer" "_m" tape-layer "_c" color tape-layer "_s" "0" "")
  tape-layer)