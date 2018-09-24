import * as functions from 'firebase-functions';

interface Config {
  qiita_user_id: string;
  qiita_access_token: string;
}

export const helloWorld = functions.https.onRequest((request, response) => {
  const config = functions.config()['qiita-timeline-rss'] as Config;

  const c = {
    qiita_user_id: config.qiita_user_id,
    qiita_access_token: (config.qiita_access_token || "").substring(0, 4)
  };

  response.send("Hello from Firebase! config=" + JSON.stringify(c));
});
