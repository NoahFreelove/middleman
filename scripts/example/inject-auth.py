import os


def transform(data):
    """Inject auth header from environment variable.

    Reads MY_API_TOKEN from the environment and adds it as a
    Bearer Authorization header. Skips injection if the variable
    is not set. Demonstrates using routePath / targetPath context
    to vary behavior per route.
    """
    token = os.environ.get("MY_API_TOKEN")
    if token is None:
        return data

    # Use route context to scope the auth scheme per target
    target = data.get("targetPath", "")
    if target.startswith("/admin"):
        header_value = "Admin " + token
    else:
        header_value = "Bearer " + token

    data["headers"].append({"name": "Authorization", "value": header_value})
    return data
