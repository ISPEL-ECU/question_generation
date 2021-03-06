# -----------------------------------------------------------
# All the datasets used in functions
# -----------------------------------------------------------

# a list containing name of all countries.
country_names = ['United States', 'Afghanistan', 'Albania', 'Algeria', 'American Samoa', 'Andorra', 'Angola',
                 'Anguilla', 'Antarctica', 'Antigua And Barbuda', 'Argentina', 'Armenia', 'Aruba', 'Australia',
                 'Austria', 'Azerbaijan', 'Bahamas', 'Bahrain', 'Bangladesh', 'Barbados', 'Belarus', 'Belgium',
                 'Belize', 'Benin', 'Bermuda', 'Bhutan', 'Bolivia', 'Bosnia And Herzegowina', 'Botswana',
                 'Bouvet Island', 'Brazil', 'Brunei Darussalam', 'Bulgaria', 'Burkina Faso', 'Burundi', 'Cambodia',
                 'Cameroon', 'Canada', 'Cape Verde', 'Cayman Islands', 'Central African Rep', 'Chad', 'Chile', 'China',
                 'Christmas Island', 'Cocos Islands', 'Colombia', 'Comoros', 'Congo', 'Cook Islands', 'Costa Rica',
                 'Cote D`ivoire', 'Croatia', 'Cuba', 'Cyprus', 'Czech Republic', 'Denmark', 'Djibouti', 'Dominica',
                 'Dominican Republic', 'East Timor', 'Ecuador', 'Egypt', 'El Salvador', 'Equatorial Guinea', 'Eritrea',
                 'Estonia', 'Ethiopia', 'Falkland Islands (Malvinas)', 'Faroe Islands', 'Fiji', 'Finland', 'France',
                 'French Guiana', 'French Polynesia', 'French S. Territories', 'Gabon', 'Gambia', 'Georgia', 'Germany',
                 'Ghana', 'Gibraltar', 'Greece', 'Greenland', 'Grenada', 'Guadeloupe', 'Guam', 'Guatemala', 'Guinea',
                 'Guinea-bissau', 'Guyana', 'Haiti', 'Honduras', 'Hong Kong', 'Hungary', 'Iceland', 'India',
                 'Indonesia', 'Iran', 'Iraq', 'Ireland', 'Israel', 'Italy', 'Jamaica', 'Japan', 'Jordan', 'Kazakhstan',
                 'Kenya', 'Kiribati', 'North Korea', 'South Korea', 'Kuwait', 'Kyrgyzstan', 'Laos', 'Latvia', 'Lebanon',
                 'Lesotho', 'Liberia', 'Libya', 'Liechtenstein', 'Lithuania', 'Luxembourg', 'Macau', 'Macedonia',
                 'Madagascar', 'Malawi', 'Malaysia', 'Maldives', 'Mali', 'Malta', 'Marshall Islands', 'Martinique',
                 'Mauritania', 'Mauritius', 'Mayotte', 'Mexico', 'Micronesia', 'Moldova', 'Monaco', 'Mongolia',
                 'Montserrat', 'Morocco', 'Mozambique', 'Myanmar', 'Namibia', 'Nauru', 'Nepal', 'Netherlands',
                 'Netherlands Antilles', 'New Caledonia', 'New Zealand', 'Nicaragua', 'Niger', 'Nigeria', 'Niue',
                 'Norfolk Island', 'Northern Mariana Islands', 'Norway', 'Oman', 'Pakistan', 'Palau', 'Panama',
                 'Papua New Guinea', 'Paraguay', 'Peru', 'Philippines', 'Pitcairn', 'Poland', 'Portugal', 'Puerto Rico',
                 'Qatar', 'Reunion', 'Romania', 'Russian Federation', 'Rwanda', 'Saint Kitts And Nevis', 'Saint Lucia',
                 'St Vincent/Grenadines', 'Samoa', 'San Marino', 'Sao Tome', 'Saudi Arabia', 'Senegal', 'Seychelles',
                 'Sierra Leone', 'Singapore', 'Slovakia', 'Slovenia', 'Solomon Islands', 'Somalia', 'South Africa',
                 'Spain', 'Sri Lanka', 'St. Helena', 'St.Pierre', 'Sudan', 'Suriname', 'Swaziland', 'Sweden',
                 'Switzerland', 'Syrian Arab Republic', 'Taiwan', 'Tajikistan', 'Tanzania', 'Thailand', 'Togo',
                 'Tokelau', 'Tonga', 'Trinidad And Tobago', 'Tunisia', 'Turkey', 'Turkmenistan', 'Tuvalu', 'Uganda',
                 'Ukraine', 'United Arab Emirates', 'United Kingdom', 'Uruguay', 'Uzbekistan', 'Vanuatu',
                 'Vatican City State', 'Venezuela', 'Viet Nam', 'Virgin Islands (British)', 'Virgin Islands (U.S.)',
                 'Western Sahara', 'Yemen', 'Yugoslavia', 'Zaire', 'Zambia', 'Zimbabwe']

# a list contains name of all capital cities.
city_names = ['Kabul', 'Tirana', 'Algiers', 'Andorra la Vella', 'Luanda', 'Saint John???s', 'Buenos Aires',
              'Yerevan', 'Canberra', 'Vienna', 'Baku', 'Nassau', 'Manama', 'Dhaka', 'Bridgetown', 'Minsk',
              'Brussels', 'Belmopan', 'Porto-Novo', 'Thimphu', 'La Paz', 'Sucre', 'Sarajevo', 'Gaborone',
              'Brasilia', 'Bandar Seri Begawan', 'Sofia', 'Ouagadougou', 'Bujumbura', 'Phnom Penh', 'Yaounde',
              'Ottawa', 'Praia', 'Bangui', 'N???Djamena', 'Santiago', 'Beijing', 'Bogota', 'Moroni',
              'Brazzaville', 'Kinshasa', 'San Jose', 'Yamoussoukro', 'Zagreb', 'Havana', 'Nicosia', 'Prague',
              'Copenhagen', 'Djibouti', 'Roseau', 'Santo Domingo', 'Dili', 'Quito', 'Cairo', 'San Salvador',
              'Malabo', 'Asmara', 'Tallinn', 'Addis Ababa', 'Suva', 'Helsinki', 'Paris', 'Libreville',
              'Banjul', 'Tbilisi', 'Berlin', 'Accra', 'Athens', 'Saint George???s', 'Guatemala City', 'Conakry',
              'Bissau', 'Georgetown', 'Port-au-Prince', 'Tegucigalpa', 'Budapest', 'Reykjavik', 'New Delhi',
              'Jakarta', 'Tehran', 'Baghdad', 'Dublin', 'Jerusalem*', 'Rome', 'Kingston', 'Tokyo', 'Amman',
              'Astana', 'Nairobi', 'Tarawa Atoll', 'Pyongyang', 'Seoul', 'Pristina', 'Kuwait City', 'Bishkek',
              'Vientiane', 'Riga', 'Beirut', 'Maseru', 'Monrovia', 'Tripoli', 'Vaduz', 'Vilnius', 'Luxembourg',
              'Skopje', 'Antananarivo', 'Lilongwe', 'Kuala Lumpur', 'Male', 'Bamako', 'Valletta', 'Majuro',
              'Nouakchott', 'Port Louis', 'Mexico City', 'Palikir', 'Chisinau', 'Monaco', 'Ulaanbaatar',
              'Podgorica', 'Rabat', 'Maputo', 'Naypyidaw', 'Windhoek', 'Yaren District', 'Kathmandu',
              'Amsterdam', 'Wellington', 'Managua', 'Niamey', 'Abuja', 'Oslo', 'Muscat', 'Islamabad',
              'Melekeok', 'Panama City', 'Port Moresby', 'Asuncion', 'Lima', 'Manila', 'Warsaw', 'Lisbon',
              'Doha', 'Bucharest', 'Moscow', 'Kigali', 'Basseterre', 'Castries', 'Kingstown', 'Apia',
              'San Marino', 'Sao Tome', 'Riyadh', 'Dakar', 'Belgrade', 'Victoria', 'Freetown', 'Singapore',
              'Bratislava', 'Ljubljana', 'Honiara', 'Mogadishu', 'Pretoria', 'Cape Town', 'Bloemfontein', 'Juba',
              'Madrid', 'Colombo', 'Sri Jayewardenepura Kotte', 'Khartoum', 'Paramaribo', 'Mbabane', 'Stockholm',
              'Bern', 'Damascus', 'Taipei', 'Dushanbe', 'Dodoma', 'Bangkok', 'Lome', 'Nuku???alofa',
              'Port-of-Spain', 'Tunis', 'Ankara', 'Ashgabat', 'Funafuti', 'Kampala', 'Kyiv', 'Abu Dhabi',
              'London', 'Washington D.C.', 'Montevideo', 'Tashkent', 'Port-Vila', 'Vatican City', 'Caracas',
              'Hanoi', 'Sanaa', 'Lusaka', 'Harare']

# a list containing variety of male names to be used in sets.
male_names = ['Saman', 'Farhad', 'Borna', 'Matin', 'Shayan', 'Farbod', 'Amir', 'Nima', 'Mohsen', 'Farzad', 'Salim',
              'Emre', 'Can', 'Mustafa', 'Tunc', 'Mesut', 'Nurcan', 'Kerem', 'Darius', 'Berat', 'Vance',
              'Ruben', 'Derick', 'Lance', 'Case', 'Reilly', 'Bryce', 'Efe', 'Allen', 'Zack', 'Matthew', 'Ezequiel',
              'Hassan', 'Shaun', 'Marvin', 'Derrick', 'Gavyn', 'Giovanni', 'Ryan', 'Ismael', 'Rudy', 'Makai', 'Ace',
              'Cooper', 'Trace', 'Darrell', 'Ali', 'Tony', 'Marquise', 'Niko']

# a list containing variety of female names to be used in sets.
female_names = ['Elif', 'Feray', 'Sevda', 'Umut', 'Dilara', 'Ece', 'Azra', 'Aylin', 'Sara', 'Ipek',
                'Aysan', 'Marlene', 'Amira', 'Tiana', 'Yasmin', 'Guadalupe', 'Paula', 'Madilynn', 'Paityn', 'Sidney',
                'Micaela', 'Jada', 'Elle', 'Kamora', 'Lillian', 'Anabelle', 'Shelby', 'Aniya', 'Aliza', 'Tatum',
                'Naomi', 'Marie', 'Kaia', 'Ava', 'Naima', 'Marisol', 'Amy', 'Kaitlynn', 'Lizeth', 'Callie', 'Miriam',
                'Alicia', 'Litzy', 'Charlize', 'Natalie', 'Ashanti', 'Jenna', 'Aliya', 'Lia', 'Savannah']

# a list containing all english characters.
characters = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U',
              'V', 'W', 'X', 'Y', 'Z']
              
              