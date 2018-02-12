declare module "node-twitter-api" {
  export = Twitter;

  class Twitter {
    constructor(options: any);

    account(type: any, params: any, accessToken: any, accessTokenSecret: any, callback: any): void;

    blocks(type: any, params: any, accessToken: any, accessTokenSecret: any, callback: any): void;

    direct_messages(type: any, params: any, accessToken: any, accessTokenSecret: any, callback: any): void;

    favorites(type: any, params: any, accessToken: any, accessTokenSecret: any, callback: any): void;

    followers(type: any, params: any, accessToken: any, accessTokenSecret: any, callback: any): void;

    friends(type: any, params: any, accessToken: any, accessTokenSecret: any, callback: any): void;

    friendships(type: any, params: any, accessToken: any, accessTokenSecret: any, callback: any): void;

    geo(type: any, params: any, accessToken: any, accessTokenSecret: any, callback: any): void;

    getAccessToken(requestToken: any, requestTokenSecret: any, oauth_verifier: any, callback: any): void;

    getAuthUrl(requestToken: any, options: any): any;

    getRequestToken(callback: any): void;

    getStream(type: any, params: any, accessToken: any, accessTokenSecret: any, dataCallback: any, endCallback: any): any;

    getTimeline(type: any, params: any, accessToken: any, accessTokenSecret: any, callback: any): any;

    help(type: any, params: any, accessToken: any, accessTokenSecret: any, callback: any): void;

    lists(type: any, params: any, accessToken: any, accessTokenSecret: any, callback: any): void;

    mutes(type: any, params: any, accessToken: any, accessTokenSecret: any, callback: any): void;

    oauth(type: any, params: any, accessToken: any, accessTokenSecret: any, callback: any): void;

    rateLimitStatus(params: any, accessToken: any, accessTokenSecret: any, callback: any): void;

    report_spam(type: any, params: any, accessToken: any, accessTokenSecret: any, callback: any): void;

    savedSearches(type: any, params: any, accessToken: any, accessTokenSecret: any, callback: any): void;

    search(params: any, accessToken: any, accessTokenSecret: any, callback: any): void;

    statuses(type: any, params: any, accessToken: any, accessTokenSecret: any, callback: any): any;

    suggestions(type: any, params: any, accessToken: any, accessTokenSecret: any, callback: any): void;

    trends(type: any, params: any, accessToken: any, accessTokenSecret: any, callback: any): void;

    updateProfileImage(params: any, accessToken: any, accessTokenSecret: any, callback: any): void;

    uploadMedia(params: any, accessToken: any, accessTokenSecret: any, callback: any): void;

    uploadMediaChunked(params: any, media_type: any, accessToken: any, accessTokenSecret: any, callback: any): any;

    uploadVideo(params: any, accessToken: any, accessTokenSecret: any, callback: any): void;

    users(type: any, params: any, accessToken: any, accessTokenSecret: any, callback: any): void;

    verifyCredentials(accessToken: any, accessTokenSecret: any, params: any, callback: any): void;

    static VERSION: string;
  }
}
