#' I noticed that I'm missing a few politicians in my politicians data 
#' collection for who I have Tweet data. Therefore, I decided to hardcode those
#' politicians' information to still be able to use them in the analysis.

df_politicians <- read_csv('Data/Politicians_Data/overview.csv')

df_house1 <- read_csv('Data/Harvard_Datasets/115th Congress/house.csv')
df_house2 <- read_csv('Data/Harvard_Datasets/116th Congress/house.csv')

additional_names <- union(unique(df_house1$user_screen_name), unique(df_house2$user_screen_name))
additional_names <- setdiff(additional_names, df_politicians$screen_name)

party_name_append <- c('Republican', 'Republican', 'Republican', 'Republican', 'Democrat', 'Republican', 
                       'Republican', 'Republican', 'Democratic', 'Republican', 'Democrat',
                       'Democrat', 'Democrat', 'Republican', 'Republican', 'Republican',
                       'Democrat', 'Republican', 'Republican', 'Republican', 'Republican',
                       'Republican', 'Democrat', 'Republican', 'Republican', 'Democrat',
                       'Republican', 'Republican', 'Republican', 'Republican', 'Republican', 'Republican',
                       'Republican', 'Republican', 'Democrat', 'Republican', 'Republican',
                       'Libertarian', 'Republican', 'Democrat', 'Democrat', 'Republican',
                       'Democrat', 'Republican', 'Republican', 'Republican', 'Republican',
                       'Republican', 'Democrat', 'Republican', 'Republican', 'Republican',
                       'Republican', 'Republican', 'Republican', 'Democrat', 'Democrat',
                       'Democrat', 'Democrat', 'Republican', 'Republican', 'Republican',
                       'Republican', 'Republican', 'Democrat', 'Republican', 'Democrat',
                       'Democrat', 'Republican', 'Republican', 'Republican', 'Republican',
                       'Republican', 'Republican', 'Republican', 'Republican', 'Republican',
                       'Republican', 'Democrat', 'Democrat', 'Republican', 'Republican',
                       'Republican', 'Republican', 'Republican', 'Republican Party',
                       'Republican Party', 'Democrat', 'Republican', 'Democrat', 'Republican',
                       'Republican', 'Republican', 'Republican', 'Republican', 'Republican',
                       'Republican', 'Republican', 'Republican', 'Democrat', 'Democrat',
                       'Republican', 'Democrat', 'Democrat', 'Democrat', 'Democrat',
                       'Democrat', 'Democrat', 'Democrat', 'Democrat', 'Democrat',
                       'Republican', 'Republican', 'Democrat', 'Democrat', 'Democrat',
                       'Democrat', 'Republican', 'Republican', 'Republican', 'Democrat',
                       'Democrat', 'Republican', 'Democrat', 'Republican', 'Democrat',
                       'Republican', 'Democrat', 'Republican', 'Democrat', 'Democrat',
                       'Democrat', 'Democrat', 'Republican', 'Democrat', 'Democrat',
                       'Democrat', 'Republican', 'Democrat', 'Republican', 'Democrat',
                       'Republican', 'Republican', 'Republican', 'Republican', 'Democrat',
                       'Republican', 'Democrat', 'Democrat', 'Democrat', 'Republican')

first_last_append <- c('Bradley Byrne', 'Steve Knight', 'Ed Royce', 'Doug Collins', 'Elizabeth Esty',
                       'Ted Yoho', 'Ileana Ros-Lehtinen', 'Scott Tipton', 'Jared Polis',
                       'Susan Brooks', 'John Delaney', 'Tim Walz', 'Keith Ellison',
                       'Walter Jones', 'Dan Donovan', 'Lou Barletta', 'Bob Brady',
                       'Jim Renacci', 'Steven Russell', 'Keith Rothfus', 'Diane Black',
                       'Ted Poe', 'Beto O\'rourke', 'Phil Roe', 'Barbara Comstock',
                       'Denny Heck', 'Bob Goodlatte', 'Ralph Abraham', 'Steve King', 'Scott Taylor',
                       'Paul Cook', 'Mike Coffman', 'Carlos Curbelo', 'Francis Rooney',
                       'Dan Lipinski', 'Todd Rokita', 'Raul Labrador', 'Justin Amash',
                       'Paul Mitchell', 'David Loebsack', 'John Lewis', 'Peter Roskam',
                       'Elijah Cummings', 'Dennis Ross', 'David Young', 'Rod Blum',
                       'Rob Woodall', 'Erik Paulsen', 'Sander Levin', 'Bruce Poliquin',
                       'Mark Meadows', 'Pete King', 'Leonard Lance', 'Tom Macarthur',
                       'Sam Johnson', 'Ruben Kihuen', 'Michelle Grisham', 'Joe Crowley', 'Jose Serrano',
                       'Will Hurd', 'Frank Lobiondo', 'Greg Walden', 'Charlie Dent',
                       'Pete Olson', 'Nita Lowey', 'Mike Conaway', 'Carol Shea-Porter',
                       'Eliot Engel', 'Trey Gowdy', 'John Faso', 'George Holding',
                       'Gregg Harper', 'Kenny Marchant', 'Mark Sanford', 'Sean Duffy',
                       'Jason Chaffetz', 'Paul Ryan', 'Jeb Hensarling', 'Luis Gutierrez',
                       'Tulsi Gabbard', 'Tom Graves', 'Dave Trott', 'Thomas Garrett',
                       'Blake Frenthold', 'John Shimkus', 'Pat Tiberi', 'Jimmy Duncan',
                       'Louise Slaughter', 'Robert Pittenger', 'John Conyers', 
                       'Jim Bridenstine', 'Joe Barton', 'Mac Thornberry', 'Tom Rooney',
                       'Devin Nunes', 'Dana Rohrabacher', 'Lamar Smith', 'Ryan Zinke',
                       'Tom Price', 'Dan Lipinski', 'Maria Horn', 'Rob Bishop',
                       'Abby Finkenauer', 'Kendra Horn', 'Katie Hill', 'Joe Cunningham',
                       'Max Rose', 'Elaine Luria', 'Xochtil Small', 'Anthony Brindisi',
                       'Donna Shalala', 'Trey Hollingsworth', 'Francis Rooney',
                       'Abby Finkenauer', 'Lacy Clay', 'Ben Mcadams', 'Pete Visclosky',
                       'Bradley Byrne', 'Tom Graves', 'Pete Olson', 'Harley Rouda',
                       'Tulsi Gabbard', 'Steve King', 'Barbara Lee', 'Ross Spano',
                       'Elijah Cummings', 'Ralph Abraham', 'Debbie Schultz',
                       'John Shimkus', 'TJ Cox', 'Katie Hill', 'Harley Rouda',
                       'TJ Cox', 'Denver Riggleman', 'Debbie Mucarsel-Powell',
                       'Gil Cisneros', 'Ben Mcadams', 'Denver Riggleman', 'David Loebsack',
                       'George Holding', 'Max Rose', 'Mac Thornberry', 'Pete King',
                       'Ross Spano', 'Greg Walden', 'Pete Visclosky', 'Will Hurd',
                       'Xochitl Small', 'MJ Hegar', 'Nate Mcmurray', 'Doug Collins')

office_title_append <- c(rep('House Representative', 148), 'House Candidate', 'House Candidate', 'House Representative')

state_append <- c('AL', 'CA', 'CA', 'GA', 'CT', 'FL', 'FL', 'CO', 'CO', 'IN', 'MD', 'MN',
                  'MN', 'NC', 'NY', 'PA', 'PA', 'OH', 'OK', 'PA', 'TN', 'TX', 'TX',
                  'TN', 'VA', 'WA', 'VA', 'LA', 'IA', 'VA', 'CA', 'CO', 'FL', 'FL', 'IL',
                  'IN', 'ID', 'MI', 'MI', 'IA', 'GA', 'IL', 'MD', 'FL', 'IA', 'IA',
                  'GA', 'MN', 'MI', 'ME', 'NC', 'NY', 'NJ', 'NJ', 'TX', 'NV', 'NM',
                  'NY', 'NY', 'TX', 'NJ', 'OR', 'PA', 'TX', 'NY', 'TX', 'NH', 'NY',
                  'SC', 'NY', 'NC', 'MS', 'TX', 'SC', 'WI', 'UT', 'WI', 'TX', 'IL',
                  'HI', 'GA', 'MI', 'VA', 'TX', 'IL', 'OH', 'TN', 'NY', 'NC', 'MI',
                  'OK', 'TX', 'TX', 'FL', 'CA', 'CA', 'TX', 'MT', 'GA', 'IL', 'CT',
                  'UT', 'IA', 'OK', 'CA', 'SC', 'NY', 'VA', 'NM', 'NY', 'FL', 'IN',
                  'FL', 'IA', 'MO', 'UT', 'IN', 'AL', 'GA', 'TX', 'CA', 'HI', 'IA',
                  'CA', 'FL', 'MD', 'LA', 'FL', 'IL', 'CA', 'CA', 'CA', 'CA', 'VA',
                  'FL', 'CA', 'UT', 'VA', 'IA', 'NC', 'NY', 'TX', 'NY', 'FL', 'OR',
                  'IN', 'TX', 'NM', 'TX', 'NY', 'GA')


df_to_append <- tibble('first_last' = first_last_append,
                       'office_title' = office_title_append,
                       'screen_name' = additional_names,
                       'party_name' = party_name_append,
                       'state' = state_append)

df_politicians <- rbind.fill(df_politicians, df_to_append)
