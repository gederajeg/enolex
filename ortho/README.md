# Workflow for curating the orthography profile.

- write a skeleton profile once (using the `write.profile()` function)

- open the skeleton profile in Libre Office

- open the Enggano transcription system from Google Spreadsheet (inside the `data` folder in the main `Enggano` Google Drive folder)

- start working (i.e., check the grapheme and common transcription) by grapheme category (e.g., vowel first, then consonant, then glides, or vice versa)

- determine the ordering of the grapheme profile for the replacement

- then, run the `tokenize()` function based on the edited skeleton profile to (i) segmentise and (ii) transliterate the grapheme (incl. the normalisation of the string)

	- the argument `regex = TRUE/FALSE` depends on the orthography profile for each author (cf. Helfrich 1888 for the contextual replacement for the allophone of \h\ into \h\ or \x\)

- tidying up the segmentised and transliterated data


