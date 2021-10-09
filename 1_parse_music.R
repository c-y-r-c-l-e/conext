
# Setup -------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(tidyr)
library(magrittr)
library(xml2)
library(stringr)

musicfile_location <- "./data-input"
musicfile_filename <- "co.musicxml"

musicfile_filepath <- file.path(musicfile_location, musicfile_filename)


# Open music file ---------------------------------------------------------

music_file <- xml2::read_xml(musicfile_filepath)

# Make a dataframe of all the notes and rests in the music
notes <- tibble(raw = as.character(xml2::xml_find_all(music_file, "//note")))
notes <- notes %>% 
  mutate(name = stringr::str_match(string = raw, 
                                   pattern = "<step>(.*?)</step>")[,2],  # [,2] is the first captured group, will be NA in case of a rest
         accidental = stringr::str_match(string = raw, 
                                         pattern = "<alter>(.*?)</alter>")[,2],
         octave = stringr::str_match(string = raw, 
                                     pattern = "<octave>(.*?)</octave>")[,2],
         staff = stringr::str_match(string = raw, 
                                    pattern = "<staff>(.*?)</staff>")[,2])

# If a note name is NA, it should be a rest
notes <- notes %>%
  # mutate(name = replace_na("r"))             # This should work, but it replaces everything with NA!
  mutate(name = replace(x = name,              # This does work. 
                        list = is.na(name), 
                        values = "r"))

# Remove xml from df
notes <- notes %>%
  mutate(raw = NULL)

# Clean up
rm(music_file)


# Add some music theory ---------------------------------------------------
# Translate musical notation to integers ([B, -1, 2] -> [Bb, 2] -> [1, 2])
# If this doesn't make any sense, try looking at a piano
translation <- tibble::tribble(
  ~from, ~to,
  "A-2",  7,      "A-1",  8,      "A",  9,      "A1", 10,      "A2", 11,
  "B-2",  9,      "B-1", 10,      "B", 11,      "B1",  0,      "B2",  1,
  "C-2", 10,      "C-1", 11,      "C",  0,      "C1",  1,      "C2",  2,
  "D-2",  0,      "D-1",  1,      "D",  2,      "D1",  3,      "D2",  4,
  "E-2",  2,      "E-1",  3,      "E",  4,      "E1",  5,      "E2",  6,
  "F-2",  3,      "F-1",  4,      "F",  5,      "F1",  6,      "F2",  7,
  "G-2",  5,      "G-1",  6,      "G",  7,      "G1",  8,      "G2",  9,
  "r",  150)


note_to_integer <- function(name, accidental) {
  fullname <- paste0(name, 
                     if_else(condition = is.na(as.character(accidental)),
                             true = "",
                             false = as.character(accidental))
                     )
  number <- unlist(sapply(fullname, function(note_as_string) {
    note_as_number <- translation %>%
      filter(from == note_as_string) %>%
      pull(to)
    return(note_as_number)
  }))
  return(number)
}

notes <- notes %>%
  mutate(note = note_to_integer(name, accidental))

# Clean up the dataframe
notes <- notes %>%
  select(staff, note, octave) %>%
  mutate(staff = as.integer(staff),
         note = as.integer(note),
         octave = as.integer(octave))

# Make absolute pitches by adding the octave
notes <- notes %>%
  mutate(octraise = 12 * octave,
         abs = note + octraise)

# Split by staff
notes_right <- notes %>% filter(staff == 1) %>% select(note, octave, abs)
notes_left  <- notes %>% filter(staff == 2) %>% select(note, octave, abs)

# Add interval from previous note
notes_right <- notes_right %>% 
  mutate(interval = abs - lag(abs)) %>% 
  mutate(interval = replace(interval, is.na(interval), 0))
notes_left  <- notes_left  %>% 
  mutate(interval = abs - lag(abs)) %>% 
  mutate(interval = replace(interval, is.na(interval), 0))

# Make another set of dataframes, aggregated as chords
chords_right <- notes_right %>%
  transform(ordinal = 1:5,                 # Recycle 1:5, but mutate() does not allow that
            measure = rep(1:(NROW(.)/5), 
                          each = 5)) %>%
  as_tibble() %>%
  pivot_wider(id_cols = measure,
              names_from = ordinal,
              values_from = c(note, abs, octave, interval))
chords_left  <- notes_left %>%
  transform(ordinal = 1:5,
            measure = rep(1:(NROW(.)/5), 
                          each = 5)) %>%
  as_tibble() %>%
  pivot_wider(id_cols = measure,
              names_from = ordinal,
              values_from = c(note, abs, octave, interval))


# An example for me to work out how to proceed
"
C  C#   D  D#   E  F  F#  G  G#  A  A#  B
    2       4          7      9     11
1       3       5  6      8      10    12

     absolutes;   note intervals;  chord's first note interval
C      1  5  8         ?   4   3          NA
Dm     3  6 10        -5   3   4           2
C      1  5  8        -9   4   3          -2
Dm     3  6 10        -5   3   4           2
E      5  9 12        -5   4   3           2
F#m    7 10 14        -5   3   4           2
E      5  9 12        -9   4   3          -2
F#m    7 10 14        -5   3   4           2
Gm     9 12 16        -5   3   4           2
G      9 13 16        -5   4   3           0
Gm     9 12 16        -5   3   4           0
G      9 13 16        -5   4   3           0
C      1  5  8       -15   4   3          -8
C2     5  8 13        -3   3   5           4
C      1  5  8       -12   4   3          -4
C2     5  8 13        -3   3   5           4
C3     8 13 17        -5   5   4           3
C2     5  8 12       -12   3   5          -3
C3     8 13 17        -4   5   4           3

In a system of three notes per measure,
 - 2nd + 3rd note intervals within chord denote a chord type: 
    - 4,3 = major,   
    - 3,4 = minor,   
    - 3,5 = major first inversion
    - 5,4 = major second inversion
    - 4,5 = minor first inversion
    - 5,3 = minor second inversion
 - interval between each measure's 1st note denotes progression
 - BUT, for inversions, that needs to be adjusted to its actual root
    (otherwise a progression from C major to C major first inversion would 
     show up as one from C major to E unknown)
"



  
# TODO: translate the following Python code
"
# Interlace right hand and left hand
# From { R0, R1, R2, R3, R4, L0, L1, L2, L3, L4, R5, R6, ... }
# To   { R0, L0, R1, L1, R2, L2, R3, L3, R4, L4, R5, L5, ... }
temp_helper = [0, 4, -1, 3, -2, 2, -3, 1, -4, 0]
temp_indices_train = [j for j in range(0, notes_train["abs"].count())]
temp_indices_valid = [j for j in range(0, notes_valid["abs"].count())]
temp_indices_test = [j for j in range(0, notes_test["abs"].count())]
temp_indices_train = [i + temp_helper[i % 10] for i in temp_indices_train]
temp_indices_valid = [i + temp_helper[i % 10] for i in temp_indices_valid]
temp_indices_test = [i + temp_helper[i % 10] for i in temp_indices_test]

notes_train_seq = notes_train["abs"][temp_indices_train]
notes_valid_seq = notes_valid["abs"][temp_indices_valid]
notes_test_seq = notes_test["abs"][temp_indices_test]

notes_train_seq.reset_index(drop=True)
notes_valid_seq.reset_index(drop=True)
notes_test_seq.reset_index(drop=True)

# Pickle the data
with open('./data_interim/pickled_data', 'wb') as f:
  pickle.dump((notes_train_seq, notes_valid_seq, notes_test_seq), f)
""