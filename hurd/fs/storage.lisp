
(in-package :hurd)

(defcenum file-storage-class
  :other
  :device
  :hurd-file
  :network
  :memory
  :task
  :zero
  :concat
  :interleave
  :layer
  :remap
  :copy)
