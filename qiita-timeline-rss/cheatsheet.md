
```sh
# To update config to set
# ``functions.config()['service-key'].entry_key``
# to entry_value.
firebase functions:config:set service-key.entry_key="entry_value"

# To deploy.
firebase deploy --project qiita-timeline-rss --only functions
```
