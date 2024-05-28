-- ---- Review Tasks ----

-- create table review_task_types (
--   id             smallint  not null,
--   name           text      not null,
--   description    text      not null,

--   constraint review_task_types_pk primary key(id)
-- );

-- create table review_task_result_types (
--   id            smallint  not null,
--   name          text      not null,
--   description   text      not null,

--   constraint review_task_result_types_pk primary key(id)
-- );

-- create table review_task_states (
--   id             smallint  not null,
--   name           text      not null,
--   description    text      not null,

--   constraint review_task_states_pk primary key(id)
-- );

-- create table review_rejection_reasons (
--   id                smallint  not null,
--   name              text      not null,
--   description       text      not null,
--   post_type_id      smallint  null,

--   constraint review_rejection_reasons_pk primary key(id),
--   constraint review_rejection_reasons__post_types__fk foreign key(post_type_id) references post_types (id)
-- );

-- create table review_tasks (
--   id                              integer   not null,
--   post_id                         integer   not null,
--   review_task_type_id             smallint  not null,
--   review_task_state_id            smallint  not null,
--   creation_date                   timestamp,
--   deletion_date                   timestamp,
--   suggested_edit_id               integer,
--   completed_by_review_task_id     integer,

--   constraint review_tasks_pk                               primary key(id),
--   constraint review_tasks__review_task_types__fk           foreign key(review_task_type_id) references review_task_types (id),
--   constraint review_tasks__review_task_states__fk          foreign key(review_task_state_id) references review_task_states (id),
--   constraint review_tasks__posts__fk                       foreign key(post_id) references posts (id),
--   constraint review_tasks__suggested_edits__fk             foreign key(suggested_edit_id) references suggested_edits (id),
--   constraint review_tasks__completed_by_review_task_id__fk foreign key(completed_by_review_task_id) references review_task_results (id)
-- );

-- create table review_task_results (
--   id                               integer  not null,
--   review_task_id                   integer  not null,
--   review_task_result_type_id       smallint not null,
--   creation_date                    timestamp,
--   rejection_reason_id              smallint,
--   comment                          text,

--   constraint review_task_results_pk                            primary key(id),
--   constraint review_task_results__review_tasks__fk             foreign key(review_task_id) references review_tasks (id),
--   constraint review_task_results__review_task_result_types__fk foreign key(review_task_result_type_id) references review_task_result_types (id),
--   constraint review_task_results__review_rejection_reasons__fk foreign key(rejection_reason_id) references review_rejection_reasons (id)
-- );
