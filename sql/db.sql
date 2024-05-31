---- Users ----

create table users (
  id                 integer     not null,
  reputation         integer     not null,
  views              integer     not null,
  upvotes            integer     not null,
  downvotes          integer     not null,
  creation_date      timestamp   not null,
  last_access_date   timestamp   not null,
  account_id         integer,
  display_name       text,
  website_url        text,
  location           text,
  about_me           text,

  constraint users_pk primary key(id)
);

create table badges (
  id           integer         not null,
  user_id      integer         not null,
  name         text            not null,
  date         timestamp       not null,
  class        smallint        not null,
  tag_based    boolean         not null,

  constraint badges_pk primary key(id),
  constraint badges__users__fk foreign key(user_id) references users(id)
);

---- Posts ----

create table post_notice_types (
  id                             integer   not null,
  post_notice_duration_id        integer   not null,
  is_hidden                      boolean   not null,
  predefined                     boolean   not null,
  classid                        smallint  not null,
  name                           text,
  body                           text,

  constraint post_notice_types_pk primary key(id)
);

create table post_types (
  id smallint not null,
  name text   not null,
  constraint post_types_pk primary key(id)
);

create table posts (
  id                              integer    not null,
  post_type_id                    smallint   not null,
  creation_date                   timestamp  not null,
  score                           integer    not null,
  view_count                      integer    null,
  comment_count                   integer    null,
  favorite_count                  integer    null,
  owner_display_name              text       null,
  last_editor_user_id             integer    null,
  last_editor_display_name        text       null,
  last_edit_date                  timestamp  null,
  deletion_date                   timestamp,
  accepted_answer_id              integer,
  parent_id                       integer,
  body                            text,
  owner_user_id                   integer,
  last_activity_date              timestamp,
  title                           text,
  tags                            text,
  answer_count                    integer,
  closed_date                     timestamp,
  community_owned_date            timestamp,

  constraint posts_pk                  primary key(id),
  constraint posts__post_types__fk     foreign key(post_type_id) references post_types (id),
  constraint posts__users__fk          foreign key(owner_user_id) references users (id),
  constraint posts__users_001__fk      foreign key(last_editor_user_id) references users (id),
  constraint posts__posts__fk          foreign key(accepted_answer_id) references posts (id),
  constraint posts__posts_001__fk      foreign key(parent_id) references posts (id)
);


create table post_notices (
  id                           integer    not null,
  post_id                      integer    not null,
  creation_date                timestamp  not null,
  post_notice_type_id          integer,
  deletion_date                timestamp,
  expiry_date                  timestamp,
  body                         text,
  owner_user_id                integer,
  deletion_user_id             integer,

  constraint post_notices_pk                 primary key(id),
  constraint post_notices__posts             foreign key(post_id) references posts (id),
  constraint post_notices__post_notice_types foreign key(post_notice_type_id) references post_notice_types (id),
  constraint post_notices__users             foreign key(owner_user_id) references users (id),
  constraint post_notices__users_001         foreign key(deletion_user_id) references users (id)
);

create table post_links (
  id                  integer     not null,
  creation_date       timestamp   not null,
  post_id             integer     not null,
  related_post_id     integer     not null,
  link_type_id        smallint    not null,

  constraint post_links_pk             primary key(id),
  constraint post_links__posts__fk     foreign key(post_id) references posts (id),
  constraint post_links__posts_001__fk foreign key(related_post_id) references posts (id)
);

create table comments (
  id                   integer     not null,
  post_id              integer     not null,
  score                integer     not null,
  text                 text        not null,
  creation_date        timestamp   not null,
  user_display_name    text,
  user_id              integer,

  constraint comments_pk         primary key(id),
  constraint comments__posts__fk foreign key(post_id) references posts (id),
  constraint comments__users__fk foreign key(user_id) references users (id)
);

---- Closed ----
create table close_reason_types (
  id            smallint  not null,
  name          text      not null,
  description   text,

  constraint close_reason_types_pk primary key(id)
);

create table posts_with_deleted (
  id                                integer     not null,
  post_type_id                      smallint    not null,
  score                             integer     not null,
  creation_date                     timestamp   not null,
  view_count                        integer     null,
  comment_count                     integer     null,
  favorite_count                    integer     null,
  owner_display_name                text        null,
  last_editor_user_id               integer     null,
  last_editor_display_name          text        null,
  last_edit_date                    timestamp   null,
  accepted_answer_id                integer,
  parent_id                         integer,
  deletion_date                     timestamp,
  body                              text,
  owner_user_id                     integer,
  last_activity_date                timestamp,
  title                             text,
  tags                              text,
  answer_count                      integer,
  closed_date                       timestamp,
  community_owned_date              timestamp,

  constraint posts_with_deleted_pk               primary key(id),
  constraint posts_with_deleted__post_types__fk  foreign key(post_type_id) references post_types (id),
  constraint posts_with_deleted__users__fk       foreign key(owner_user_id) references users (id),
  constraint posts_with_deleted__users_001__fk   foreign key(last_editor_user_id) references users (id),
  constraint posts_with_deleted__posts__fk       foreign key(accepted_answer_id) references posts (id),
  constraint posts_with_deleted__posts_001__fk   foreign key(parent_id) references posts (id)
);

create table close_as_offtopic_reason_types (
  id                                    smallint   not null,
  is_universal                          boolean    not null,
  markdown_mini                         text       not null,
  creation_date                         timestamp  not null,
  creation_moderator_id                 integer,
  approval_date                         timestamp,
  approval_moderator_id                 integer,
  deactivation_date                     timestamp,
  deactivation_moderator_id             integer,

  constraint close_offtopic_reason_types_pk     primary key(id),
  constraint close_offtopic_types__users        foreign key(creation_moderator_id) references users (id),
  constraint close_offtopic_types__users_001    foreign key(approval_moderator_id) references users (id),
  constraint close_offtopic_types__users_002    foreign key(deactivation_moderator_id) references users (id)
);

---- Flags ----

create table flag_types (
  id            smallint not null,
  name          text     not null,
  description   text     not null,

  constraint flag_types_pk primary key(id)
);

create table pending_flags (
  id                                       integer   not null,
  flag_type_id                             smallint  not null,
  post_id                                  integer   not null,
  creation_date                            timestamp,
  close_reason_type_id                     smallint,
  close_as_offtopic_reason_type_id         smallint,
  duplicate_of_question_id                 integer,
  belongs_on_base_host_address             text,

  constraint pending_flags_pk                           primary key(id),
  constraint pending__flag_types__fk                    foreign key(flag_type_id) references flag_types (id),
  constraint pending__posts__fk                         foreign key(post_id) references posts (id),
  constraint pending__close_reason_types__fk            foreign key(close_reason_type_id) references close_reason_types (id),
  constraint pending__close_as_offtopic_reason_types__fk foreign key(close_as_offtopic_reason_type_id) references close_as_offtopic_reason_types (id),
  constraint pending__posts_001__fk                     foreign key(duplicate_of_question_id) references posts (id)
);


---- Votes and Feedback ----

create table votes (
  id              integer   not null,
  post_id         integer   not null,
  vote_type_id    smallint  not null,
  user_id         integer,
  creation_date   timestamp,
  bounty_amount   integer
);

create table vote_types (
  id    smallint not null,
  name  text not null,

  constraint vote_types_pk primary key(id)
);

create table suggested_edits (
  id                 integer    not null,
  post_id            integer    not null,
  approval_date      timestamp  null,
  rejection_date     timestamp  null,
  creation_date      timestamp,
  owner_user_id      integer,
  comment            text,
  text               text,
  title              text,
  tags               text,
  revision_guid      uuid,

  constraint suggested_edits_pk         primary key(id),
  constraint suggested_edits__posts__fk foreign key(post_id) references posts (id),
  constraint suggested_edits__users__fk foreign key(owner_user_id) references users (id)
);

create table suggested_edit_votes (
  id                    integer    not null,
  suggested_edit_id     integer    not null,
  user_id               integer    not null,
  target_rep_change     integer    not null,
  vote_type_id          smallint   not null,
  creation_date         timestamp  not null,
  target_user_id        integer,


  constraint suggested_edit_votes_pk  primary key(id),
  constraint sev__suggested_edits__fk foreign key(suggested_edit_id) references suggested_edits (id),
  constraint sev__users__fk           foreign key(user_id) references users (id),
  constraint sev__vote_types__fk      foreign key(vote_type_id) references vote_types (id),
  constraint sev__users_001__fk       foreign key(target_user_id) references users (id)
);

create table post_feedback (
  id                integer    not null,
  post_id           integer    not null,
  vote_type_id      smallint   not null,
  creation_date     timestamp  not null,
  is_anonymous      boolean,

  constraint post_feedback_pk              primary key(id),
  constraint post_feedback__posts__fk      foreign key(post_id) references posts (id),
  constraint post_feedback__vote_types__fk foreign key(vote_type_id) references vote_types (id)
);


---- Tags ----

create table tags (
  id                  integer not null,
  count               integer not null,
  excerpt_post_id     integer null,
  wiki_post_id        integer null,
  tag_name            text,


  constraint tags_pk             primary key(id),
  constraint tags_name_unique    unique(tag_name),
  constraint tags__posts__fk     foreign key(excerpt_post_id) references posts (id),
  constraint tags__posts_001__fk foreign key(wiki_post_id) references posts (id)
);

create table post_tags (
  post_id integer not null,
  tag_id  integer not null,

  constraint post_tags__posts__fk foreign key(post_id) references posts (id),
  constraint post_tags__tags__fk  foreign key(tag_id) references tags (id)
);

create table tag_synonyms (
  id                       integer    not null,
  auto_rename_count        integer    not null,
  score                    integer    not null,
  creation_date            timestamp  not null,
  owner_user_id            integer    null,
  source_tag_name          text       null,
  target_tag_name          text       null,
  last_auto_rename         timestamp  null,
  approved_by_user_id      integer    null,
  approval_date            timestamp  null,

  constraint tag_synonyms_pk                        primary key(id),
  constraint tag_synonyms__users__fk                foreign key(owner_user_id) references users (id),
  constraint tag_synonyms__users_001__fk            foreign key(approved_by_user_id) references users (id),
  constraint tag_synonyms__source_tag_name_tags__fk foreign key(source_tag_name) references tags (tag_name),
  constraint tag_synonyms__target_tag_name_tags__fk foreign key(target_tag_name) references tags (tag_name)
);

---- History ----

create table post_history_types (
  id smallint not null,
  name text not null,
  constraint post_history_types_pk primary key(id)
);

create table post_history (
  id                        integer    not null,
  post_id                   integer    not null,
  post_history_type_id      smallint   not null,
  revision_guid             uuid       not null,
  creation_date             timestamp  not null,
  user_id                   integer,
  user_display_name         text,
  comment                   text,
  text                      text,

  constraint post_history_pk                      primary key(id),
  constraint post_history__post_history_types__fk foreign key(post_history_type_id) references post_history_types (id),
  constraint post_history__posts__fk              foreign key(post_id) references posts (id),
  constraint post_history__users__fk              foreign key(user_id) references users (id)
);
