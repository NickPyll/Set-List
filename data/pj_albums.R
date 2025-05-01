# studio albums ----

album.dark_matter <- data.frame(
  song_name = c(
    "Scared of Fear", "React, Respond", "Wreckage", "Dark Matter", "Won't Tell", "Upper Hand", "Waiting for Stevie",
    "Running", "Something Special", "Got to Give", "Setting Sun"
  )
) |>
  mutate(release_date = "2024-04-19", album_detail = "", album = "Dark Matter")

album.gigaton <- data.frame(
  song_name = c(
    "Who Ever Said", "Superblood Wolfmoon", "Dance of the Clairvoyants", "Quick Escape", "Alright", "Seven O'Clock",
    "Never Destination", "Take the Long Way", "Buckle Up", "Comes Then Goes", "Retrograde", "River Cross"
  )
) |>
  mutate(release_date = "2020-03-27", album_detail = "", album = "Gigaton")

album.lightning_bolt <- data.frame(
  song_name = c(
    "Getaway", "Mind Your Manners", "My Father's Son", "Sirens", "Lightning Bolt", "Infallible", "Pendulum",
    "Swallowed Whole", "Let the Records Play", "Sleeping by Myself", "Yellow Moon", "Future Days"
  )
) |>
  mutate(release_date = "2013-10-13", album_detail = "", album = "Lightning Bolt")

album.backspacer <- data.frame(
  song_name = c(
    "Gonna See My Friend", "Got Some", "The Fixer", "Johnny Guitar", "Just Breathe", "Amongst the Waves",
    "Unthought Known", "Supersonic", "Speed of Sound", "Force of Nature", "The End"
  )
) |>
  mutate(release_date = "2009-09-20", album_detail = "", album = "Backspacer")

album.pearl_jam <- data.frame(
  song_name = c(
    "Life Wasted", "World Wide Suicide", "Comatose", "Severed Hand", "Marker in the Sand", "Parachutes",
    "Unemployable", "Big Wave", "Gone", "Wasted Reprise", "Army Reserve", "Come Back", "Inside Job"
  )
) |>
  mutate(release_date = "2006-05-02", album_detail = "", album = "Pearl Jam")

album.riot_act <- data.frame(
  song_name = c(
    "Can't Keep", "Save You", "Love Boat Captain", "Cropduster", "Ghost", "I Am Mine", "Thumbing My Way",
    "You Are", "Get Right", "Green Disease", "Help Help", "Bu$hleaguer", "1/2 Full", "Arc", "All or None"
  )
) |>
  mutate(release_date = "2002-10-12", album_detail = "", album = "Riot Act")

album.binaural <- data.frame(
  song_name = c(
    "Breakerfall", "Gods' Dice", "Evacuation", "Light Years", "Nothing as It Seems", "Thin Air", "Insignificance",
    "Of the Girl", "Grievance", "Rival", "Sleight of Hand", "Soon Forget", "Parting Ways"
  )
) |>
  mutate(release_date = "2000-05-16", album_detail = "", album = "Binaural")

album.yield <- data.frame(
  song_name = c(
    "Brain of J.", "Faithfull", "No Way", "Given to Fly", "Wishlist", "Pilate", "Do the Evolution", "Red Bar",
    "MFC", "Low Light", "In Hiding", "Push Me, Pull Me", "All Those Yesterdays"
  )
) |>
  mutate(release_date = "1998-02-03", album_detail = "", album = "Yield")

album.no_code <- data.frame(
  song_name = c(
    "Sometimes", "Hail, Hail", "Who You Are", "In My Tree", "Smile", "Off He Goes", "Habit", "Red Mosquito",
    "Lukin", "Present Tense", "Mankind", "I'm Open", "Around the Bend"
  )
) |>
  mutate(release_date = "1996-08-27", album_detail = "", album = "No Code")

album.vitalogy <- data.frame(
  song_name = c(
    "Last Exit", "Spin the Black Circle", "Not for You", "Tremor Christ", "Nothingman", "Whipping", "Pry, To",
    "Corduroy", "Bugs", "Satan's Bed", "Better Man", "Aye Davanita", "Immortality", "Hey Foxymophandlemama, That's Me"
  )
) |>
  mutate(release_date = "1994-11-22", album_detail = "", album = "Vitalogy")

album.vs <- data.frame(
  song_name = c(
    "Go", "Animal", "Daughter", "Glorified G", "Dissident", "W.M.A.", "Blood", "Rearviewmirror", "Rats",
    "Elderly Woman Behind the Counter in a Small Town", "Leash", "Indifference"
  )
) |>
  mutate(release_date = "1993-10-19", album_detail = "", album = "Vs")

album.ten <- data.frame(
  song_name = c("Once", "Even Flow", "Alive", "Why Go", "Black", "Jeremy", "Oceans", "Porch", "Garden", "Deep", "Release")
) |>
  mutate(release_date = "1991-08-27", album_detail = "", album = "Ten")

# b sides and such ----

album.bsides <-
  tribble(
    ~song_name, ~release_date, ~album_detail,
    "Wash", "1991-07-07", "Alive single",
    "Let Me Sleep", "1991-12-31", "Ten Club Christmas single",
    "Dirty Frank", "1992-03-30", "Even Flow single",
    "Yellow Ledbetter", "1992-08-17", "Jeremy single",
    "Footsteps", "1992-08-17", "Jeremy single",
    "Alone", "1993-10-25", "Go single",
    "Out of My Mind", "1995-02-13", "Not for You single",
    "I Got Id", "1995-12-04", "Merkin Ball",
    "Long Road", "1995-12-04", "Merkin Ball",
    "Black, Red, Yellow", "1996-10-21", "Hail, Hail single",
    "Leatherman", "1997-12-22", "Given to Fly single",
    "U", "1998-05-05", "Wishlist single",
    "Untitled", "1998-11-24", "Live on Two Legs",
    "Down", "2002-10-08", "I Am Mine single",
    "Undone", "2002-10-08", "I Am Mine single",
    "All Night", "2003-11-11", "Lost Dogs",
    "Bee Girl", "2003-11-11", "Lost Dogs",
    "Don't Gimme No Lip", "2003-11-11", "Lost Dogs",
    "Fatal", "2003-11-11", "Lost Dogs",
    "Hold On", "2003-11-11", "Lost Dogs",
    "Sad", "2003-11-11", "Lost Dogs",
    "Sweet Lew", "2003-11-11", "Lost Dogs",
    "Other Side", "2003-02-11", "Save You single",
    "Of the Earth", "2010-01-01", "unreleased",
    "Can't Deny Me", "2018-03-10", "Digital release",
    "Evil Little Goat", "2009-03-24", "Ten Redux"

    # not sure what to do with these yet
    # 'Summer Sky'
    # 'Yellow Ledbetter / Little Wing'
    # 'Improvisation'
  ) |>
  mutate(album = "Other")

# soundtracks ----

album.soundtracks <-
  tribble(
    ~song_name, ~release_date, ~album_detail,
    "Breath", "1992-06-30", "Singles soundtrack",
    "State of Love and Trust", "1992-06-30", "Singles soundtrack",
    "Hard to Imagine", "1998-08-25", "Chicago Cab soundtrack",
    "Man of the Hour", "2003-12-23", "Big Fish soundtrack"
  ) |>
  mutate(album = "Other")

levels.album <-
  c(
    "Ten", "Vs", "Vitalogy", "No Code",
    "Yield", "Binaural", "Riot Act", "Pearl Jam",
    "Backspacer", "Lightning Bolt", "Gigaton", "Dark Matter",
    "Other"
  )

colors.album <-
  colorRampPalette(
    colors =
      c(
        "#A53F60", "#E67C3D", "#1D1D1D", "#BDC5CB",
        "#D75C43", "#70FA7B", "#F3D483", "#266392",
        "#535058", "#B81A1D", "#415975", "#95871d",
        "grey42"
      )
  )(13)

linetypes.album <-
  c(
    "solid", "solid", "solid", "solid",
    "dashed", "solid", "solid", "solid",
    "dashed", "dotted", "dotted", "dashed",
    "solid"
  )
