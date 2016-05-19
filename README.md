# datetime

Datetime is a library for parsing, formatting, matching and recoding
timestamps and date-time format strings.  It is written as a utility
for Logview, so currently most of functionality not needed for that
mode is *not implemented*.


### Pattern types

There exist several different ways to specify date and/or time format.
Different programming language and their libraries use different
formats.  E.g. format of date 2015-09-24 can be expressed at least in
the following ways:

    * `yyyy-MM-dd` ([ICU](http://userguide.icu-project.org/formatparse/datetime), [Java](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html))

    * `%Y-%m-%d` (POSIX, Emacs)

    * `Y-m-d` ([PHP](http://php.net/manual/en/function.date.php))

This library currently uses Java pattern format, but is internally
written in such a way that support for other types can be added
relatively easily.

