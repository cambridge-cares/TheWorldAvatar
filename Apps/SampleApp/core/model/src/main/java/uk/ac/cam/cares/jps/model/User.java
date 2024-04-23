package uk.ac.cam.cares.jps.model;

/**
 * This is a model class which structure follows the result JSON from https://jsonplaceholder.typicode.com/users/{id}
 * But it only keeps id, name, username and email.
 * The original response is shown as below
 * {
 *     "id": 1,
 *     "name": "Leanne Graham",
 *     "username": "Bret",
 *     "email": "Sincere@april.biz",
 *     "address": {
 *         "street": "Kulas Light",
 *         "suite": "Apt. 556",
 *         "city": "Gwenborough",
 *         "zipcode": "92998-3874",
 *         "geo": {
 *             "lat": "-37.3159",
 *             "lng": "81.1496"
 *         }
 *     },
 *     "phone": "1-770-736-8031 x56442",
 *     "website": "hildegard.org",
 *     "company": {
 *         "name": "Romaguera-Crona",
 *         "catchPhrase": "Multi-layered client-server neural-net",
 *         "bs": "harness real-time e-markets"
 *     }
 * }
 */
public class User {
    String id;
    String name;
    String username;
    String email;

    public User(String id, String name, String username, String email) {
        this.id = id;
        this.name = name;
        this.username = username;
        this.email = email;
    }

    public String getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public String getUsername() {
        return username;
    }

    public String getEmail() {
        return email;
    }
}
