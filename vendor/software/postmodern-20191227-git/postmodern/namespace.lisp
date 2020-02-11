;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN; -*-
(in-package :postmodern)

(defmacro with-schema ((schema &key (strict t) (if-not-exist :create) (drop-after nil))
                       &body form)
  "A macro to set the schema search path of the postgresql
   database to include as first entry a specified schema.

   calling with strict 't only the specified schema is set as current
   search path. All other schema are then not searched any more.

   calling with if-not-exist set to :create the schema is created if
   this schema did not exist.

   calling with drop-after set to 't the schema is removed after the
   execution of the body form.

   example :
     (with-schema (:schema-name :strict nil :drop-after nil :if-not-exist :error)
            (foo 1)
            (foo 2))"
  `(do-with-schema ,schema (lambda () ,@form)
     :strict ,strict :if-not-exist ,if-not-exist :drop-after ,drop-after))

(defun do-with-schema (schema thunk &key strict if-not-exist drop-after)
  (let ((old-search-path (get-search-path)))
    (unwind-protect
         (progn
           (unless (schema-exists-p schema)
             (if (eq if-not-exist :create)
                 (create-schema schema)
                 (error 'database-error :message (format nil "Schema '~a' does not exist." schema))))
           (set-search-path (if strict (to-sql-name schema t) (concatenate 'string (to-sql-name schema t) "," old-search-path)))
           (funcall thunk))
      (set-search-path old-search-path)
      (when drop-after (drop-schema schema :cascade 't)))))

(defun get-search-path ()
  "Returns the default schema search path for the current session."
  (query "SHOW search_path" :single))

(defun set-search-path (path)
  "This changes the postgresql runtime parameter controlling what order
schemas are searched. You can always use fully qualified names [schema.table].
By default, this function only changes the search path for the current session."
  (execute (format nil "SET search_path TO ~a" path)))

(defun list-schemata ()
  "List all existing user defined schemata.

  Note: The query uses the portable information_schema relations instead of pg_tables relations
  SELECT schema_name FROM information_schema.schemata where schema_name !~ '(pg_*)|information_schema' ORDER BY schema_name ;"
  (query (:select 'schema_name
          :from 'information_schema.schemata
          :where (:!~ 'schema_name "pg_.*|information_schema"))
         :column))

(defun list-schemas ()
  "List schemas in the current database, excluding the pg_* system schemas."
  (loop for x in (query (:select 'nspname
                                 :from 'pg_namespace
                                 :where (:!~* 'nspname "^pg_.*")))
       collect (first x)))


(defun schema-exists-p (name)
  "Predicate for schema existence. More consistent with naming scheme for other functions."
  (query (:select (:exists (:select 'schema_name
                            :from 'information_schema.schemata
                            :where (:= 'schema_name '$1))))
         (to-sql-name name)
         :single))

(defun create-schema (schema)
  "Creating a non existing schema.
   If the schema exists an error is raised."
  ;;(format t "creating schema: ~a" schema)
  (execute (format nil "CREATE SCHEMA ~a" (s-sql:to-sql-name schema t))))

(defun drop-schema (schema &key (if-exists nil) (cascade nil))
  "Drops an existing database schema 'schema'
A notice instead of an error is raised with the is-exists parameter."
  (execute (format nil "DROP SCHEMA ~:[~;IF EXISTS~] ~a ~:[~;CASCADE~]" if-exists (s-sql:to-sql-name schema t) cascade)))
