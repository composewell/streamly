## Overview

Unicode is a standard which consists of a `char set`, `encodings`, attributes
and properties of characters, processing of strings, paragraphs, processing of
text in `locale` specific manner.

### Char Set

Unicode `char set` represents characters from all languages in the world.  Each
character is assigned a, code point, a unique number identifying the character,
and written as `U+0076` where the four hex digits `0076` represent the unique
number assigned to the character.

### Encodings

A unicode character can be encoded as a:

* fixed length encoding with a 32-bit value directly representing the code
  point in little endian (UTF32LE) or big endian (UTF32BE) byte ordering.  See
  https://en.wikipedia.org/wiki/UTF-32.
* variable length encoding with one or two 16-bit values depending on the code
  point, UTF16LE and UTF16BE. See https://en.wikipedia.org/wiki/UTF-16.
* variable length encoding with one, two or three 8-bit values depending on the
  code point, UTF8. See https://en.wikipedia.org/wiki/UTF-8.

### i18n and L10n

Internationalization (i18n) is being able to represent and process all
languages in the world. Unicode performs i18n by representing all languages and
their common processing rules.

Localization (L10n) is being able to customize the common internationalized
processing to a country or region (`locale`). Unicode specifies various
standard locales which includes customization of the attributes and processing
rules for each locale. Custom locales can be created with custom text
processing rules.

* https://en.wikipedia.org/wiki/Internationalization_and_localization .
* http://userguide.icu-project.org/i18n

## POSIX Locales

On Debian Linux, the default system wide locale can be administered using
`localectl` or `sudo dpkg-reconfigure locales`.

In a shell, the `locale` command shows the current locale settings. When you
start a program from the shell it inherits these settings via the process
environment and the C library loads and uses the appropriate locale. Even some
GUI programs if started from the shell can use the values from the
environment. Other GUI programs may have there own locale settings that can be
configured from their menu.

The following environment variables can override the system wide locale and
determine how a process (handled by libc) performs unicode text processing for
different locale aspects:

```
LC_CTYPE     Defines character classification and case conversion.
LC_COLLATE   Defines collation rules.
LC_MONETARY  Defines the format and symbols used in formatting of monetary information.
LC_NUMERIC   Defines the decimal delimiter, grouping, and grouping symbol for non-monetary numeric editing.
LC_TIME      Defines the format and content of date and time information.
LC_MESSAGES  Defines the format and values of affirmative and negative responses.
```

Each environment variable above can be set to available `locale` settings. For
example `LC_CTYPE=en_US.UTF-8` (locale.charmap) specifies a locale `en_US` for
the language `en` (English) and the territory `US` (USA) to be used for
character classifications (e.g. `iswalpha`) and case conversions (e.g.
`toupper`). `UTF-8` is the charmap used by the locale.

`C` and `POSIX` locales are the same and are used by default. glibc also
provides a generic `i18n` locale.

Use `locale -a` command to see all available locales on a system and `locale
-m` for charmaps. See `man locale` as well. Also see
`/usr/share/i18n/SUPPORTED` on Linux (Debian).
https://www.gnu.org/software/libc/manual/html_node/Locale-Names.html for the
format in which these environment variables can be specified.

The environment variables are preferred in the following order:

```
LC_ALL       Overrides everything else
LC_*         Individual aspect customization
LANG         Used as a substitute for any unset LC_* variable
```

Normally only `LANG` should be used. If a particular aspect needs to be
cutsomized then `LC_*` variables can be used. `LC_ALL` overrides everything.
On GNU/Linux `LANGUAGE` can also be used as a list of preferred languages
separated by ":". LANGUAGE is effective only if LANG has been set to something
other than default and has higher priority than anything else.

* https://www.gnu.org/software/gettext/manual/gettext.html#Users has a good
  overview of locale settings.
* https://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap07.html
  POSIX Locales standard

### Localizing Messages

GNU https://www.gnu.org/software/gettext/manual/gettext.html can be used by
programs to translate/localize the user interfacing text to multiple languages.
Catalogs of localized program error, alert, notification messages are installed
at e.g.  `/usr/share/locale/en_US/LC_MESSAGES/`.

* https://www.gnu.org/software/gettext/manual/gettext.html

### Creating Locales

From the `debian` manpage of `localedef` POSIX command:

The  localedef  program  reads  the  indicated charmap and input files,
compiles them to a binary form quickly usable by the locale functions in the C
library (setlocale(3), localeconv(3), etc.), and places the output in
outputpath.

See `locale-gen` for a more high level program to generate locales.
/usr/lib/locale/locale-archive contains the generated binary files. On Debian
you can use `sudo dpkg-reconfigure locales` to select locales and set system
locale.

On Linux/glibc (Debian), installed `charmaps` can be found at
`/usr/share/i18n/charmaps/` and locale definition input files at
`/usr/share/i18n/locales/`. 

## ICU Locales

An ICU locale is frequently confused with a POSIX locale ID. An ICU locale ID
is not a POSIX locale ID. ICU locales do not specify the encoding and specify
variant locales differently.

* http://userguide.icu-project.org/locale

### Localizing Messages

* http://userguide.icu-project.org/locale/localizing

## Unicode Text processing

* http://userguide.icu-project.org/posix
* http://userguide.icu-project.org/strings/properties

### Locale Independent

1) Char Properties
2) Normalization
3) Regex matching

### Locale Specific

1) Case Mapping:
    * https://unicode.org/faq/casemap_charprop.html
    * http://www.unicode.org/versions/latest/ch05.pdf#G21180
    * ftp://ftp.unicode.org/Public/UCD/latest/ucd/SpecialCasing.txt
    * ftp://ftp.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt
2) Breaking
3) Collation
4) Charset Conversion

## Haskell

### Haskell Unicode Text Processing

Related packages on hackage:

* [base](https://www.stackage.org/lts/package/base) Data.Char module, uses libc
* [text](https://www.stackage.org/lts/package/text)
* [text-icu](https://stackage.org/lts/package/text-icu) C bindings to icu


* https://github.com/composewell/unicode-transforms
* https://github.com/llelf/prose
* [unicode-properties](https://hackage.haskell.org/package/unicode-properties) Unicode 3.2.0 character properties
* [hxt-charproperties](http://www.stackage.org/lts/package/hxt-charproperties) Character properties and classes for XML and Unicode
* [unicode-names](http://hackage.haskell.org/package/unicode-names) Unicode 3.2.0 character names
* [unicode](https://hackage.haskell.org/package/unicode) Construct and transform unicode characters


* [charset](https://www.stackage.org/lts/package/charset) Fast unicode character sets

None of the existing Haskell packages provide comprehensive and fast access to
properties. `unicode-transforms` provides composition/decomposition data and
the script to extract data from unicode database.

### Haskell Localization

* http://wiki.haskell.org/Internationalization_of_Haskell_programs
* https://hackage.haskell.org/package/localize
* http://hackage.haskell.org/package/localization
* http://hackage.haskell.org/package/hgettext
* http://hackage.haskell.org/package/i18n
* https://hackage.haskell.org/package/setlocale
* http://hackage.haskell.org/package/system-locale
* https://github.com/llelf/numerals

## TODO

Factor out a `unicode-data` package from `unicode-transforms`.
`unicode-transforms` package will depend on unicode-data and can continue to be
used as is. Other packages can take advantage of the `unicode-data` to provide
unicode text processing services.

To begin with, this package will contain:

* char properties data
* case mapping data
* unicode normalization data

This package can be used in `Streamly.Internal.Data.Unicode.*` to provide:

* Fast access to char properties, we will no longer depend on libc for that and
  no FFI will be required to do iswspace, iswalpha etc.
* Correct case mappings from a single char to multi-char
* Stream based unicode normalization
* Breaking (locale independent for now)

## Later

* add locale data from CLDR to `unicode-data` to provide locale specific
  services as well.
* support locale specific breaking, regex, collation and charset conversion
* facility to add customized locale data
* Support providing application level resource data for localization
