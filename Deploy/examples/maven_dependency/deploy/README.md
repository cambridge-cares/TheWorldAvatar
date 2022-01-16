# Example: deploying an artifact with Maven

This directory contains a minimal working example for deploying binary files as a Maven artifact.
To use it, you'll need to have Maven installed and a configuration file (at <user_home>/.m2/settings.xml)
containing a server block and repository block with ID='worldavatar-github'. That is:

```
<server>
    <id>worldavatar-github</id>
    <username>USERNAME</username>
    <password>ENCRYPTED_GITHUB_TOKEN</password>
</server>
```
and

```
<repository>
    <id>worldavatar-github</id>
    <name>World Avatar github</name>
    <url>https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/</url>
    <layout>default</layout>
    <releases>
        <enabled>true</enabled>
    </releases>
    <snapshots>
        <enabled>true</enabled>
    </snapshots>
</repository>
```
Note that, to deploy to the World Avatar Maven repository, you'll need to generate a github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token), which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).  You'll then need to encrypt that token; see [these instructions](https://maven.apache.org/guides/mini/guide-encryption.html) for details.

## Installing and deploying the artifact

To construct the artifact and install it locally:

```
mvn install
```

This zips up the contents of ./model_files and installs it to your local Maven repository at <user_home>/.m2/repository/com/example/model_files/<version_number>/

To deploy the zip file to worldavatar-github:

```
mvn deploy
```

---

Note that, redeployment is disabled for non-snapshot version numbers (those without a '-SNAPSHOT' suffix) on the World Avatar Maven repository.
Attempting to deploy such an artifact when it already exists on the server will fail with the error message 'Failed to deploy artifacts'.

---

Once you've deployed your files, [this example](../use/README.md) shows how they can be downloaded and built into an image using Docker.