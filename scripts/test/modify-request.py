def transform(data):
    """Add X-Modified-By header to the request."""
    data["headers"].append({"name": "X-Modified-By", "value": "python-script"})
    return data
