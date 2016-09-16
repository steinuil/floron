(div (@ (class index))
     (p (time post-date) " | "
        (a (@ (href post-link)) post-title)
        (p (@ (class description)) post-desc)))
