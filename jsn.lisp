
(defpackage :jsn
  (:use cl)
  (:use postmodern)
  (:local-nicknames (:rc :redis))
  (:use cl-json)
  (:export :jsnresponse)
  )
(in-package :jsn)

(defvar *DICT* "{\"id\":5385,\"name\":\"Neymar Cunha\",\"grade\":\"4ª\",\"classroom\":\"\",\"balance\":\"247.84\",\
 \"available_limit_for_today\":\"3.0\",\"birthday\":\"2002-12-31\",\"does_not_use_password_on_terminal\":false,\"gender\":\"male\",\
 \"last_active_card\":{\"id\":50074,\"number\":\"500740\",\"track2\":\";00668533=076161270570492005385?\n\"},\
 \"login\":null,\"negative_limit\":\"0.0\",\"pdv\":false,\"post_paid\":false,\
 \"registration_number\":\"00006001\",\"restricted_stores\":[],\"restrictions\":[],  \"terminal_password\":\"1234\",
 \"imported\":true,\"most_consumed_products_ids\":[],\"activated_at\":\"2015-01-25T00:00:00.000-02:00\"},{\"id\":5386,\"name\":\"Selton Silva\",\"grade\":    \"6ª\",\"classroom\":\"\",\
 \"balance\":\"-88.2\",\"available_limit_for_today\":\"-1.0\",\"birthday\":\"2005-08-15\",\"does_not_use_password_on_terminal\":false,\"gender\":\"male\",\
 \"last_active_card\":{\"id\":5462,\"number\":\"T15S5386\",\"track2\":\";00747550=076160170529108005386?\n\"},\"login\":null,\
 \"negative_limit\":\"0.0\",\"pdv\":false,\"post_paid\":false,\"registration_number\":\"00006002\",\"restricted_stores\":[],\"restrictions\":[],              \"terminal_password\":\"1234\",\"imported\":true,\"most_consumed_products_ids\":[],\"activated_at\":\"2015-10-15T00:00:00.000-03:00\"}");
(defvar *DICTS* *DICT*)
(dotimes (number 30)
  (setq *DICTS* (concatenate 'string *DICTS* "," *DICT*))
  )
(setq *DICTS* (concatenate 'string "[" *DICTS* "]"))
(setq *DICT* (json:decode-json-from-string *DICTS*))
(defun jsnresponse()
  (let ((now (get-internal-real-time))(text "") )
    (encode-json-to-string *DICTS*)
    (setq text (write-to-string (- (get-internal-real-time) now)))
    `(200 (:content-type "text/html") (,text))
    )
  )
