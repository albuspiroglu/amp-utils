;; mylib/utils/amp-scripts.lisp


(in-package #:amp-scripts)


(defun move-path-to-backup (dir-path)
  (let ((new-name (make-pathname :directory (pathname-directory dir-path)
                                 :name (concatenate 'string (pathname-name dir-path)
                                                    (format nil "~a" (local-time:now))))))
    (if (osicat:directory-exists-p new-name)
        (error "backup dir exists: ~a" new-name)
        (osicat-posix:rename dir-path new-name))))


(defun create-symlinks-to-items (links-to-folder &optional (links-from-folder "~/"))
  "For each item under links-to-folder, do one of the below in links-from-folder:
  . create symbolic link to, if a local folder doesn't exist
  . if it exists, move local folder to folder_datetime and create link afterwards"
  (dolist (target-path (osicat:list-directory links-to-folder))
    (let ((from-path (make-pathname :directory (pathname-directory links-from-folder)
                                    :name (enough-namestring (osicat:pathname-as-file target-path) links-to-folder))))
      (when (or (osicat:directory-exists-p from-path)
                (probe-file from-path))
        (move-path-to-backup from-path))
      (osicat:make-link from-path :target target-path :hard nil))))

(defun create-file (folder-or-pathname &optional fname)
  (let (path)
    (typecase folder-or-pathname
      (pathname (setf path folder-or-pathname))
      (t (setf path (make-pathname :directory folder-or-pathname :name (string fname)))))
    (with-open-file (out path
                         :direction :output
                         :if-does-not-exist :create)
      (declare (ignore out))
      nil)))

(defun create-symlinks-to-items.test1 ()
  (let ((files '("file1" "file2" ".file3"))
        (dirs '("dir1/" "dir2/" ".dir3/"))
        (temp-dir #P"/tmp/create-symlinks-to-items.test1/")
        (new-home #P"/tmp/create-symlinks-to-items.test1/new-home/")
        (shared-home #P"/tmp/create-symlinks-to-items.test1/shared-home/"))

    ;; arrange
    (osicat:delete-directory-and-files temp-dir :if-does-not-exist :ignore)
    (dolist (dir (list new-home shared-home))
      (ensure-directories-exist dir))
    (dolist (f files)
      (create-file (merge-pathnames shared-home f)))
    (dolist (d dirs)
      (ensure-directories-exist (amp-pathnames:join-paths shared-home d)))

    ;; act
    (create-symlinks-to-items shared-home new-home)

    ;; assert
    (dolist (file files)
      (let ((link-path (make-pathname :directory (pathname-directory new-home) :name file))
            (target-path (make-pathname :directory (pathname-directory shared-home) :name file)))
        (unless (osicat:good-symlink-exists-p link-path)
          (format t "symlink doesn't exist: ~a~%" file))
        (unless (equal (osicat:read-link link-path) target-path)
          (format t "symlink doesn't point to right place: ~a -> ~a~%"
                  (osicat:read-link link-path) target-path))))
    ;; cleanup
    #+(and)(osicat:delete-directory-and-files temp-dir :if-does-not-exist :ignore)))
  
