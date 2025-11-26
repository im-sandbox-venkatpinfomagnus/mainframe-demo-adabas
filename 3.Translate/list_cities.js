// create a function to list cities with their names and populations
function listCities(cities) {
    // iterate through the cities array
    for (let city of cities) {
        // log the city name and population
        console.log(`City: ${city.name}, Population: ${city.population}`);
    }
}
// create an array of city objects
const cities = [
    { name: "New York", population: 8419600 },
    { name: "Los Angeles", population: 3980400 },
    { name: "Chicago", population: 2716000 }
];

// display the list of cities in the console
console.log("Listing cities with their populations:");
// call the function to list cities
listCities(cities);
console.log("City listing completed successfully.");



