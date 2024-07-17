# Workflow for curating the orthography profile.

This is just my own (Gede Primahadi W. Rajeg) personal note to do the orthography profiling (recorded in [this code file](https://github.com/engganolang/enolex/blob/main/codes/r-code_04-1-orthography-profiling.R "Orthography R code")) so that I won't forget!

-   write a skeleton profile once (using the `write.profile()` function)

    -   commit the skeleton profile to git for track the changes

-   open the skeleton profile in Libre Office

-   open the Enggano transcription system from Google Spreadsheet (inside the `data` folder in the main `Enggano` Google Drive folder)

    -   filter by the `source` column, then

    -   filter by the `grapheme` column (esp. exclude the \[Blanks\] cell for the grapheme)

-   start working (i.e., check the grapheme and common transcription) by grapheme category (e.g., vowel first, then consonant, then glides, or vice versa)

-   determine the ordering of the grapheme profile for the replacement

    -   sequence put first

    -   then grapheme with context (left and/or right)

    -   then the rest

    -   in the function, the order is NULL (and regex is TRUE)

-   then, run the `tokenize()` function based on the edited skeleton profile to (i) segmentise and (ii) transliterate the grapheme (incl. the normalisation of the string)

    -   the argument `regex = TRUE/FALSE` depends on the orthography profile for each author (cf. Helfrich 1888 for the contextual replacement for the allophone of \h\ into \h\ or \x)
    -   when the argument `regex = TRUE`, it will capture/detect the regex in these columns: "Left", "Grapheme", "Right" of the profile skeleton file. **IMPORTANT**: we in particular need regex escape character for special character in the "Grapheme" column for the `regex = TRUE` works. Again, see the example in the [skeleton profile of Helfrich 1888](https://github.com/engganolang/enolex/blob/main/ortho/_08-helfrich1888_profile-skeleton.tsv "Helfrich (1888) skeleton profile") (esp. lines no. 19, 20, 22, and 23).

-   tidying up the segmentised and transliterated data

# Notes for the database

-   The `id` column indicates cognates grouping IDs!

-   The `entry_id` column indicates the unique IDs for each row in the database.

    -   The idea is to allow the matching-back of the separate, saved transliteration for each list to the main database.

-   The `transliterated` column contains the characters that have been transliterated and appeared in tokenised forms.

    -   The non-tokenised, transliterated characters are in the `commons` column.

-   The `ipa_tokenised` column contains the IPA phoneme in tokenised forms. The phoneme is based on the matching between the common transcription character and their corresponding phonemes as laid out in the [Enggano transcription/orthography file](https://enggano.ling-phil.ox.ac.uk/static/Enggano-transcriptions.xlsx "Enggano Transcription System").

    -   The non-tokenised, IPA-transliterated characters are in the `ipa` column.

# Notes for individual list

## Francis (1870)

-   The orthography is modelled after Oudemans (1879) because the list from Francis (a British) appears in Oudemans (1879) list.

## Helfrich (1888)

-   Uses Helfrich and Pieters (1891) orthography

## Stokhof (1987) Holle List

-   ā is transliterated into long a (which is "aa", in Enggano common transcription). The long a characteristic of ā is indicated on the first page of the Enggano list (Sub-section 1.2.1)

-   ê in the database is a typo from the original ĕ in the Enggano list (Notes 51 page 203). That is why ê is transliterated into ė, which is the transliteration for ĕ

-   ē is characterised as in the first syllable of Dutch "geven" (cf. Sub-section 1.2.1 on the first page of Enggano list), which in IPA, the "e" part is long (source: https://en.wiktionary.org/wiki/geven); that is why ē is transliterated into ee for long vowels.

-   ĕ is the orthography for schwa, hence transliterated into ė (Enggano common transcription for schwa). This is available in the first page of the Enggano list (which represents the second syllable of Dutch "de", and "geven"), and in the Stokhof (1980: 76) orthography table.

-   é is transliterated into E (Enggano common transcription for the sound /ɛ/) because it is characterised as the e in Dutch "pet" (cf. Sub-section 1.2.1 on the first page of Enggano list).

    -   Then, è is also transliterated into E because Stokhof (1980: 76, 6.2) characterises è as in Dutch "pet" representing the phonetic symbol \[ɛ\].

-   ō is transliterated as long o (oo for Enggano common transcription) because it is characterised as the o in Dutch "hopen", which is a long vowel (source: https://en.wiktionary.org/wiki/hopen)

-   ô in the database is a typo from the original ŏ in the Enggano list (ID 1326 page 199). That is why ô is transliterated into O, which is the transliteration for ŏ that is similar as o. This is illustrated with Dutch "rol", which is phonetically an \[ɔ\] (source: https://en.wiktionary.org/wiki/rol), and in Enggano common transcription, the sound /ɔ/ is transcribed with O.

## Capell (1982)

There is an issue with transliterating these characters:

```         
> cp82$missing
  Grapheme Frequency Codepoint        UnicodeName
1         ̲        36    U+0332 COMBINING LOW LINE
```

```         
> cp82$missing
  Grapheme Frequency Codepoint                     UnicodeName
1        ẽ         2    U+1EBD LATIN SMALL LETTER E WITH TILDE
```

What I did was as follows:

-   For the combining low line, I copied from this site: https://unicode-explorer.com/c/0332. So, this character is included as a separate entry in the grapheme and replaced with nothing

-   For the latin small letter e with tilde, I copied from this: <https://graphemica.com/%E1%BA%BD>. So this character is included as a separate entry in the grapheme and replaced with itself

## Kähler (1987)

-   I replaced the grapheme "ə" into "u̇" because in the Enggano transcription, there is orthography explicit for "ə" and it is replaced in common orthography with "u̇"

    -   question I have: I do not know which grapheme in Kähler (1987) needs to be replaced into "ė" in common orthography because in the common orthography guideline, the grapheme is only mentioned as "(possibly o)".

-   I also replaced the grapheme "ə́" into "u̇́"

-   I also replaced the grapheme "ɨ" into "u̇" (and "ɨ̃" into "u̇̃")

-   Then I replaced "ə̃" with "ė̃" because there is a phoneme "ə̃" for "ė̃" in the common transcription guidelines

## Kasim et al. (1987)

-   It is not included in Enggano Transcription System yet.

-   The basic guidelines (in Indonesian) are available in Kasim et al. (1987: 22)

# Note for combining the orthography and ACD

-   we need to run script processing the db (01), the ACD (03), and the orthography (04) to get the updated entry ID that may change from the db (01). It is because the orthography and ACD includes information about entry ID for re-matching.
