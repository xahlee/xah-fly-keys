;; -*- coding: utf-8 -*-

(defun xah-dired-2zip ()
  "Zip the current file in `dired'.
If multiple files are marked, only zip the first one.
Require unix zip command line tool."
  (interactive)
  (require 'dired)
  (let ( (fileName (elt (dired-get-marked-files) 0)))
    (shell-command (format "zip -r '%s.zip' '%s'" (file-relative-name fileName) (file-relative-name fileName)))))

(defun xah-process-image (φfile-list φargs-str φnew-name-suffix φnew-name-file-suffix )
  "Create a new image.
φfile-list is a list of image file paths.
φargs-str is argument string passed to ImageMagick's “convert” command.
φnew-name-suffix is the string appended to file. e.g. “_new” gets you “…_new.jpg”
φnew-name-file-suffix is the new file's file extension. e.g. “.png”
Requires ImageMagick shell command."
  (require 'dired)
  (mapc
   (lambda (ξf)
     (let ( newName cmdStr )
       (setq newName (concat (file-name-sans-extension ξf) φnew-name-suffix φnew-name-file-suffix) )
       (while (file-exists-p newName)
         (setq newName (concat (file-name-sans-extension newName) φnew-name-suffix (file-name-extension newName t))) )

       ;; relative paths used to get around Windows/Cygwin path remapping problem
       (setq cmdStr
             (format "convert %s '%s' '%s'" φargs-str (file-relative-name ξf) (file-relative-name newName)) )
       (shell-command cmdStr)
       ))
   φfile-list ))

(defun xah-dired-scale-image (φfile-list φscale-percentage φsharpen?)
  "Create a scaled version of images of marked files in dired.
The new names have “-s” appended before the file name extension.

If `universal-argument' is given, output is PNG format. Else, JPG.

When called in lisp code,
 φfile-list is a list.
 φscale-percentage is a integer.
 φsharpen? is true or false.

Requires ImageMagick unix shell command."
  (interactive
   (let (
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((string-equal major-mode "image-mode") (list (buffer-file-name)))
           (t (list (read-from-minibuffer "file name:"))))))
     (list myFileList
           (read-from-minibuffer "Scale %:")
           (y-or-n-p "Sharpen"))))
  (let ((sharpenOrNo (if φsharpen? "-sharpen 1" "" ))
        (outputSuffix (if current-prefix-arg ".png" ".jpg" )))
    (xah-process-image φfile-list
                       (format "-scale %s%% -quality 85%% %s " φscale-percentage sharpenOrNo)
                       "-s" outputSuffix )))

(defun xah-dired-image-autocrop (φfile-list φoutput-image-type-suffix)
  "Create a new auto-cropped version of images of marked files in dired.
Requires ImageMagick shell command.

If `universal-argument' is given, output is PNG format. Else, JPG."
  (interactive
   (let (
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((string-equal major-mode "image-mode") (list (buffer-file-name)))
           (t (list (read-from-minibuffer "file name:")))))
         (φoutput-image-type-suffix (if current-prefix-arg ".png" ".jpg" )))
     (list myFileList φoutput-image-type-suffix)))
  (xah-process-image φfile-list "-trim" "-cropped" φoutput-image-type-suffix ))

(defun xah-dired-2png (φfile-list)
  "Create a png version of images of marked files in dired.
Requires ImageMagick shell command."
  (interactive
   (let (
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((string-equal major-mode "image-mode") (list (buffer-file-name)))
           (t (list (read-from-minibuffer "file name:"))))))
     (list myFileList)))
  (xah-process-image φfile-list "" "-2" ".png" ))

(defun xah-dired-2drawing (φfile-list φgrayscale-p φbits-per-pixel)
  "Create a png version of (drawing type) images of marked files in dired.
Requires ImageMagick shell command."
  (interactive
   (let (
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((string-equal major-mode "image-mode") (list (buffer-file-name)))
           (t (list (read-from-minibuffer "file name:"))))))
     (list myFileList
           (setq φgrayscale-p (yes-or-no-p "Grayscale?"))
           (read-string "Bits per pixel (1 2 4 8):" "4"))))
  (xah-process-image φfile-list
                     (format "+dither %s -depth %s"
                             (if φgrayscale-p "-type grayscale" "")
                             ;; image magick “-colors” must be at least 8
                             ;; (if (< (string-to-number φbits-per-pixel) 3)
                             ;;     8
                             ;;     (expt 2 (string-to-number φbits-per-pixel)))
                             φbits-per-pixel)  "-2" ".png" )
  )

(defun xah-dired-2jpg (φfile-list)
  "Create a JPG version of images of marked files in dired.
Requires ImageMagick shell command."
  (interactive
   (let (
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((string-equal major-mode "image-mode") (list (buffer-file-name)))
           (t (list (read-from-minibuffer "file name:") )) ) ) )
     (list myFileList) )
   )
  (xah-process-image φfile-list "" "-2" ".jpg" ))

(defun xah-dired-crop-image (φfile-list)
  " .......
Requires ImageMagick shell command."
  (interactive
   (let (
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((string-equal major-mode "image-mode") (list (buffer-file-name)))
           (t (list (read-from-minibuffer "file name:"))))))
     (list myFileList)))
  (xah-process-image φfile-list "-crop 690x520+220+165" "_n" ".png" ))
