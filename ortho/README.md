# Workflow for curating the orthography profile.

This is just my own (Gede Primahadi W. Rajeg) personal note to do the orthography profiling so that I won't forget!

-   write a skeleton profile once (using the `write.profile()` function)

-   open the skeleton profile in Libre Office

-   open the Enggano transcription system from Google Spreadsheet (inside the `data` folder in the main `Enggano` Google Drive folder)

-   start working (i.e., check the grapheme and common transcription) by grapheme category (e.g., vowel first, then consonant, then glides, or vice versa)

-   determine the ordering of the grapheme profile for the replacement

-   then, run the `tokenize()` function based on the edited skeleton profile to (i) segmentise and (ii) transliterate the grapheme (incl. the normalisation of the string)

    -   the argument `regex = TRUE/FALSE` depends on the orthography profile for each author (cf. Helfrich 1888 for the contextual replacement for the allophone of \h\ into \h\ or \x)
    -   when the argument `regex = TRUE`, it will capture/detect the regex in these columns: "Left", "Grapheme", "Right" of the profile skeleton file. **IMPORTANT**: we in particular need regex escape character for special character in the "Grapheme" column for the `regex = TRUE` works. Again, see the example in the [skeleton profile of Helfrich 1888](https://github.com/engganolang/enolex/blob/main/ortho/_08-helfrich1888_profile-skeleton.tsv "Helfrich (1888) skeleton profile") (esp. lines no. 19, 20, 22, and 23).

-   tidying up the segmentised and transliterated data
