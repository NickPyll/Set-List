# studio albums ----

album.dark_matter <- data.frame(
  song_name = c("Scared of Fear", "React, Respond", "Wreckage", "Dark Matter", "Won't Tell", "Upper Hand", "Waiting for Stevie",
               "Running", "Something Special", "Got to Give", "Setting Sun")) |> 
  mutate(year_released = 2024, album_detail = '', album = 'Dark Matter')

album.gigaton <- data.frame(
  song_name = c("Who Ever Said", "Superblood Wolfmoon", "Dance of the Clairvoyants", "Quick Escape", "Alright", "Seven O'Clock",
               "Never Destination", "Take the Long Way", "Buckle Up", "Comes Then Goes", "Retrograde", "River Cross")) |> 
  mutate(year_released = 2020, album_detail = '', album = 'Gigaton')

album.lightning_bolt <- data.frame(
  song_name = c("Getaway", "Mind Your Manners", "My Father's Son", "Sirens", "Lightning Bolt", "Infallible", "Pendulum", 
               "Swallowed Whole", "Let the Records Play", "Sleeping by Myself", "Yellow Moon", "Future Days")) |> 
  mutate(year_released = 2013, album_detail = '', album = 'Lightning Bolt')

album.backspacer <- data.frame(
  song_name = c("Gonna See My Friend", "Got Some", "The Fixer", "Johnny Guitar", "Just Breathe", "Amongst the Waves", 
               "Unthought Known", "Supersonic", "Speed of Sound", "Force of Nature", "The End")) |> 
  mutate(year_released = 2009, album_detail = '', album = 'Backspacer')

album.pearl_jam <- data.frame(
  song_name = c("Life Wasted", "World Wide Suicide", "Comatose", "Severed Hand", "Marker in the Sand", "Parachutes", 
               "Unemployable", "Big Wave", "Gone", "Wasted Reprise", "Army Reserve", "Come Back", "Inside Job")) |> 
  mutate(year_released = 2006, album_detail = '', album = 'Pearl Jam')
        
album.riot_act <- data.frame(
  song_name = c("Can't Keep", "Save You", "Love Boat Captain", "Cropduster", "Ghost", "I Am Mine", "Thumbing My Way", 
               "You Are", "Get Right", "Green Disease", "Help Help", "Bu$hleaguer", "1/2 Full", "Arc", "All or None")) |> 
  mutate(year_released = 2002, album_detail = '', album = 'Riot Act')
    
album.binaural <- data.frame(
  song_name = c("Breakerfall", "Gods' Dice", "Evacuation", "Light Years", "Nothing as It Seems", "Thin Air", "Insignificance", 
               "Of the Girl", "Grievance", "Rival", "Sleight of Hand", "Soon Forget", "Parting Ways")) |> 
  mutate(year_released = 2000, album_detail = '', album = 'Binaural')
    
album.yield <- data.frame(
  song_name = c("Brain of J.", "Faithfull", "No Way", "Given to Fly", "Wishlist", "Pilate", "Do the Evolution", "Red Bar", 
               "MFC", "Low Light", "In Hiding", "Push Me, Pull Me", "All Those Yesterdays")) |> 
  mutate(year_released = 1998, album_detail = '', album = 'Yield')
    
album.no_code <- data.frame(
  song_name = c("Sometimes", "Hail, Hail", "Who You Are", "In My Tree", "Smile", "Off He Goes", "Habit", "Red Mosquito", 
               "Lukin", "Present Tense", "Mankind", "I'm Open", "Around the Bend")) |> 
  mutate(year_released = 1996, album_detail = '', album = 'No Code')
    
album.vitalogy <- data.frame(
  song_name = c("Last Exit", "Spin the Black Circle", "Not for You", "Tremor Christ", "Nothingman", "Whipping", "Pry, To", 
               "Corduroy", "Bugs", "Satan's Bed", "Better Man", "Aye Davanita", "Immortality", "Hey Foxymophandlemama, That's Me")) |> 
  mutate(year_released = 1994, album_detail = '', album = 'Vitalogy')
    
album.vs <- data.frame(
  song_name = c("Go", "Animal", "Daughter", "Glorified G", "Dissident", "W.M.A.", "Blood", "Rearviewmirror", "Rats", 
               "Elderly Woman Behind the Counter in a Small Town", "Leash", "Indifference")) |> 
  mutate(year_released = 1993, album_detail = '', album = 'Vs')

album.ten <- data.frame(
  song_name = c("Once", "Even Flow", "Alive", "Why Go", "Black", "Jeremy", "Oceans", "Porch", "Garden", "Deep", "Release")) |> 
  mutate(year_released = 1991, album_detail = '', album = 'Ten')
       
# b sides and such ----

album.bsides <-
  tribble(
    ~song_name, ~year_released, ~album_detail,
    'Wash', 1991, 'Alive single',
    'Let Me Sleep', 1991, 'Ten Club Christmas single',
    'Yellow Ledbetter', 1992, 'Jeremy single',
    'Footsteps', 1992, 'Jeremy single',
    'Alone', 1993, 'Go single',
    'Out of My Mind', 1995, 'Not for You single',
    'I Got Id', 1995, 'Merkin Ball',
    'Long Road', 1995, 'Merkin Ball',
    'Black, Red, Yellow', 1996, "Hail, Hail single",
    'Leatherman', 1997, 'Given to Fly single',
    'U', 1998, 'Wishlist single',
    'Untitled', 1998, 'Live on Two Legs',
    'Down', 2002, 'I Am Mine single',
    'Undone', 2002, 'I Am Mine single',
    'All Night', 2003, 'Lost Dogs',
    'Bee Girl', 2003, 'Lost Dogs',
    "Don't Gimme No Lip", 2003, 'Lost Dogs',
    'Fatal', 2003, 'Lost Dogs',
    'Hold On', 2003, 'Lost Dogs',
    'Sad', 2003, 'Lost Dogs',
    'Sweet Lew', 2003, 'Lost Dogs',
    'Other Side', 2003, 'Save You single',
    'Of the Earth', 2010, 'unreleased',
    "Can't Deny Me", 2018, 'Digital release',
    'Evil Little Goat', 2009, 'Ten Redux'

    # not sure what to do with these yet
    # 'Summer Sky'
    # 'Yellow Ledbetter / Little Wing'
    # 'Improvisation'
  ) |> 
  mutate(album = 'Other') 

# soundtracks ----

album.soundtracks <-
  tribble(
    ~song_name, ~year_released, ~album_detail,
    "Breath",  1992, 'Singles soundtrack',
    "State of Love and Trust", 1992, 'Singles soundtrack',
    'Hard to Imagine', 1997, 'Chicago Cab soundtrack',
    'Man of the Hour', 2003, 'Big Fish soundtrack'
  ) |>
  mutate(album = 'Other')

levels.album <- 
  c('Ten', 'Vs', 'Vitalogy', 'No Code', 
    'Yield', 'Binaural', 'Riot Act', 'Pearl Jam', 
    'Backspacer', 'Lightning Bolt', 'Gigaton', 'Dark Matter', 
    'Other')

colors.album <- 
  colorRampPalette(
    colors =
      c('#A53F60', '#E67C3D', '#1D1D1D', '#BDC5CB', 
        '#D75C43', '#70FA7B', '#F3D483', '#266392',
        '#535058', '#B81A1D', '#415975', '#95871d',
        'grey42'))(13)

linetypes.album <-
  c('solid', 'solid', 'solid', 'solid', 
    'dashed', 'solid', 'solid', 'solid',
    'dashed', 'dotted', 'dotted', 'dashed',
    'solid')
