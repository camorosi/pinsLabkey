# pinsLabkey 0.2.1

* Fixed a small bug in `labkey_check_permissions()`. Additionally, this function will now throw an error (rather than a warning) if permissions cannot be listed; i.e. if an incorrect `LABKEY_API_KEY` is provided

# pinsLabkey 0.2.0

* Remove `board_alias` argument and replace with `cache_alias` argument. Default labkey pin cache location now uses both `base_url` domain and basename of folder path. 

# pinsLabkey 0.1.0

* First development version

