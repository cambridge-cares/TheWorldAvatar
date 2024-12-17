package uk.ac.cam.cares.jps.model;

/**
 * This is a model class which structure follows the result JSON from https://jsonplaceholder.typicode.com/users/{id}
 * But it only keeps id, name, username and email.
 * The original response is shown as below
 * {
 * "id": 1,
 * "name": "Leanne Graham",
 * "username": "Bret",
 * "email": "Sincere@april.biz",
 * "address": {
 * "street": "Kulas Light",
 * "suite": "Apt. 556",
 * "city": "Gwenborough",
 * "zipcode": "92998-3874",
 * "geo": {
 * "lat": "-37.3159",
 * "lng": "81.1496"
 * }
 * },
 * "phone": "1-770-736-8031 x56442",
 * "website": "hildegard.org",
 * "company": {
 * "name": "Romaguera-Crona",
 * "catchPhrase": "Multi-layered client-server neural-net",
 * "bs": "harness real-time e-markets"
 * }
 * }
 */
public class User {
    private final String id;
    private final String name;
    private final String username;
    private final String email;

    /**
     * Constructor
     *
     * @param id       ID returned from the API.
     * @param name     User's name returned from the API.
     * @param username Username returned from the API.
     * @param email    User's email returned from the API.
     */
    public User(String id, String name, String username, String email) {
        this.id = id;
        this.name = name;
        this.username = username;
        this.email = email;
    }

    /**
     * Getter method to return ID.
     *
     * @return ID returned from the API.
     */
    public String getId() {
        return this.id;
    }

    /**
     * Getter method to return user's name.
     *
     * @return User's name returned from the API.
     */
    public String getName() {
        return this.name;
    }

    /**
     * Getter method to return username.
     *
     * @return Username returned from the API.
     */
    public String getUsername() {
        return this.username;
    }

    /**
     * Getter method to return user's email.
     *
     * @return User's email returned from the API.
     */
    public String getEmail() {
        return this.email;
    }
}
