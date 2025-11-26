# create a function to list cities with their names and populations
def list_cities(cities):
    # iterate through the cities array
    for city in cities:
        # log the city name and population
        print(f"City: {city['name']}, Population: {city['population']}")

# create an array of city objects
cities = [
    {"name": "New York", "population": 8419600},
    {"name": "Los Angeles", "population": 3980400},
    {"name": "Chicago", "population": 2716000}
]

# display the list of cities in the console
print("Listing cities with their populations:")
# call the function to list cities
list_cities(cities)
print("City listing completed successfully.")
