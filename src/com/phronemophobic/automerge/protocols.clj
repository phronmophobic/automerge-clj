(ns com.phronemophobic.automerge.protocols)


(defprotocol IItem
  (put! [item k v])
  (->clj [item])
  (root-item [item]))

(defprotocol IDocument
  (merge! [dest src])
  (clone [doc])
  (commit!
    [doc]
    [doc message t])
  (empty-change [doc message t])
  (fork
    [doc]
    [doc heads])
  (get-actor-id [doc])
  ;; (get-heads [doc])
  ;; (load-incremental [doc src count])
  (save [doc])
  (set-actor-id [doc actor-id]))

