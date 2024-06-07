#### Warning: this is not yet in a usable state for others.

-------------------------------------------------------------------------------

# stack-overflow-builder


`stack-overflow-builder` is a tool for importing the Stack Exchange Data Dump into a PostgreSQL database. Currently tested and supported on the [2024-04-02 data dump](https://archive.org/details/stackexchange_20240402_bis):

```
magnet:?xt=urn:btih:2ef5246c89679a43977b3b75eb6ab48bb15c73ae&dn=April%202024%20Stack%20Exchange%20Data%20Dump%20(fixed)
```

Files needed:

```
$ tree stackexchange_20240402_bis

stackexchange_20240402_bis
├── stackoverflow.com-Badges.7z
├── stackoverflow.com-Comments.7z
├── stackoverflow.com-PostLinks.7z
├── stackoverflow.com-Posts.7z
├── stackoverflow.com-Tags.7z
├── stackoverflow.com-Users.7z
└── stackoverflow.com-Votes.7z
```

See also:

- [Information about the Stack Exchange Data Dump](https://meta.stackexchange.com/questions/224873/all-stack-exchange-data-dumps/224922#224922)
- [Database schema documentation for the public data dump](https://meta.stackexchange.com/questions/2677/database-schema-documentation-for-the-public-data-dump-and-sede)
