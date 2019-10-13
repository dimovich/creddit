(ns creddit.core
  (:require [looper.client :as client]
            [clojure.string :as string]
            [jsonista.core :as json]
            [slingshot.slingshot :refer [try+]]))


(def json-mapper (json/object-mapper
                  {:decode-key-fn true}))


(defn- parse-response
  [response]
  (if-let [coll (or (get-in response [:data :children])
                    (get-in response [:data :trophies]))]
    (map :data coll)
    (:data response)))

(defn- valid-limit? [limit]
  (if (and (integer? limit)
           (<= 1 limit)
           (>= 100 limit))
    limit
    (throw
      (ex-info "Invalid limit - Must be an integer between 1 & 100."
               {:causes :invalid-limit}))))

(defn- valid-time? [time]
  (if (and (keyword? time)
           (contains? #{:hour :day :week :month :year :all} time))
    time
    (throw
      (ex-info "Invalid time - Must be one of the following: :hour, :day, :week, :month, :year, :all."
               {:causes :invalid-time}))))

(defn get-access-token
  [credentials]
  (try+
   (-> (client/post "https://www.reddit.com/api/v1/access_token"
                    {:basic-auth [(:user-client credentials) (:user-secret credentials)]
                     :headers {"User-Agent" (or (:user-agent credentials) "creddit")}
                     :form-params {:grant_type "client_credentials"
                                   :device_id (str (java.util.UUID/randomUUID))}
                     :content-type "application/x-www-form-urlencoded"
                     ;;:socket-timeout 10000
                     ;;:conn-timeout 10000
                     })
       (get :body)
       (json/read-value json-mapper))
   
   (catch [:status 401] {}
     (throw
      (ex-info "Unauthorised, please check your credentials are correct."
               {:causes :unauthorised})))))



(defn- http-req [method credentials url & [opts]]
  (-> (method url
              (merge
               {:headers {"User-Agent" (:user-agent credentials)
                          "Authorization" (str "bearer " (:access-token credentials))}}
               opts))
      :body
      (json/read-value json-mapper)))


(defn- http-get [credentials url & [opts]]
  (http-req client/get credentials url opts))


(defn- http-post [credentials url & [opts]]
  (http-req client/post credentials url opts))




(defn search
  [credentials params]
  (if (valid-limit? (:limit params))
    (-> (http-get credentials "https://oauth.reddit.com/search"
                   {:query-params params})
        (parse-response))))


(defn frontpage
  [credentials limit time]
  (if (and (valid-limit? limit) (valid-time? time))
    (-> (http-get credentials (str "https://www.reddit.com/.json?limit=" limit "&t=" (name time)))
        (parse-response))))

(defn controversial
  [credentials limit time]
  (if (and (valid-limit? limit) (valid-time? time))
    (-> (http-get credentials (str "https://www.reddit.com/controversial/.json?limit=" limit "&t=" (name time)))
        (parse-response))))

(defn new
  [credentials limit time]
  (if (and (valid-limit? limit) (valid-time? time))
    (-> (http-get credentials (str "https://www.reddit.com/new/.json?limit=" limit "&t=" (name time)))
        (parse-response))))

(defn rising
  [credentials limit time]
  (if (and (valid-limit? limit) (valid-time? time))
    (-> (http-get credentials (str "https://www.reddit.com/rising/.json?limit=" limit "&t=" (name time)))
        (parse-response))))

(defn top
  [credentials limit time]
  (if (and (valid-limit? limit) (valid-time? time))
    (-> (http-get credentials (str "https://www.reddit.com/top/.json?limit=" limit "&t=" (name time)))
        (parse-response))))

(defn subreddit
  [credentials subreddit limit time]
  (if (and (valid-limit? limit) (valid-time? time))
    (-> (http-get credentials (str "https://www.reddit.com/r/" subreddit "/.json?limit=" limit "&t=" (name time)))
        (parse-response))))

(defn subreddit-controversial
  [credentials subreddit limit time]
  (if (and (valid-limit? limit) (valid-time? time))
    (-> (http-get credentials (str "https://www.reddit.com/r/" subreddit "/controversial/.json?limit=" limit "&t=" (name time)))
        (parse-response))))

(defn subreddit-new
  [credentials subreddit limit time]
  (if (and (valid-limit? limit) (valid-time? time))
    
    (-> (http-get credentials (str "https://oauth.reddit.com/r/" subreddit "/new/")
                  {:query-params {:t time
                                  :limit limit}})
        #_(http-get credentials (str "https://www.reddit.com/r/" subreddit "/new/.json?limit=" limit "&t=" (name time)))
        (parse-response))))

(defn subreddit-rising
  [credentials subreddit limit time]
  (if (and (valid-limit? limit) (valid-time? time))
    (-> (http-get credentials (str "https://www.reddit.com/r/" subreddit "/rising/.json?limit=" limit "&t=" (name time)))
        (parse-response))))

(defn subreddit-top
  [credentials subreddit limit time]
  (if (and (valid-limit? limit) (valid-time? time))
    (-> (http-get credentials (str "https://www.reddit.com/r/" subreddit "/top/.json?limit=" limit "&t=" (name time)))
        (parse-response))))

(defn subreddit-comments
  [credentials subreddit limit]
  (if (valid-limit? limit)
    (-> (http-get credentials (str "https://www.reddit.com/r/" subreddit "/comments/.json?limit=" limit))
        (parse-response))))


(defn subreddit-search
  [credentials subreddit query limit]
  (if (valid-limit? limit)
    (-> (http-get credentials (str "https://oauth.reddit.com/r/" subreddit "/search/" )
                  {:query-params {:q query
                                  :limit limit}})
        (parse-response))))


(defn subreddit-about
  [credentials subreddit]
  (-> (http-get credentials (str "https://www.reddit.com/r/" subreddit "/about/.json"))
      (parse-response)))

(defn subreddit-moderators
  [credentials subreddit]
  (-> (http-get credentials (str "https://www.reddit.com/r/" subreddit "/about/moderators/.json"))
      :data
      :children))

(defn subreddits
  [credentials limit]
  (if (valid-limit? limit)
    (-> (http-get credentials (str "https://www.reddit.com/subreddits/.json?limit=" limit))
        (parse-response))))

(defn subreddits-new
  [credentials limit]
  (if (valid-limit? limit)
    (-> (http-get credentials (str "https://www.reddit.com/subreddits/new/.json?limit=" limit))
        (parse-response))))

(defn subreddits-popular
  [credentials limit]
  (if (valid-limit? limit)
    (-> (http-get credentials (str "https://www.reddit.com/subreddits/popular/.json?limit=" limit))
        (parse-response))))

(defn subreddits-gold
  [credentials limit]
  (if (valid-limit? limit)
    (-> (http-get credentials (str "https://www.reddit.com/subreddits/gold/.json?limit=" limit))
        (parse-response))))

(defn subreddits-default
  [credentials limit]
  (if (valid-limit? limit)
    (-> (http-get credentials (str "https://www.reddit.com/subreddits/default/.json?limit=" limit))
        (parse-response))))

(defn subreddits-search
  [credentials subreddit limit]
  (if (valid-limit? limit)
    (-> (http-get credentials (str "https://oauth.reddit.com/subreddits/search/")
                  {:query-params {:q subreddit
                                  :limit limit}})
        (parse-response))))

(defn user
  [credentials username]
  (-> (http-get credentials (str "https://www.reddit.com/user/" username "/about/.json"))
      (parse-response)))

(defn user-posts
  [credentials username limit time]
  (if (and (valid-limit? limit) (valid-time? time))
    (-> (http-get credentials (str "https://www.reddit.com/user/" username "/submitted/.json?limit=" limit "&t=" (name time)))
        (parse-response))))

(defn user-comments
  [credentials username limit time]
  (if (and (valid-limit? limit) (valid-time? time))
    (-> (http-get credentials (str "https://www.reddit.com/user/" username "/comments/.json?limit=" limit "&t=" (name time)))
        (parse-response))))

(defn user-trophies
  [credentials username]
  (-> (http-get credentials (str "https://www.reddit.com/user/" username "/trophies/.json"))
      (parse-response)))

(defn users
  [credentials limit]
  (if (valid-limit? limit)
    (-> (http-get credentials (str "https://www.reddit.com/users/.json?limit=" limit))
        (parse-response))))

(defn users-new
  [credentials limit]
  (if (valid-limit? limit)
    (-> (http-get credentials (str "https://www.reddit.com/users/new/.json?limit=" limit))
        (parse-response))))

(defn users-popular
  [credentials limit]
  (if (valid-limit? limit)
    (-> (http-get credentials (str "https://www.reddit.com/users/popular/.json?limit=" limit))
        (parse-response))))

(defn listing
  [credentials names]
  (-> (http-get credentials (str "https://www.reddit.com/by_id/" (string/join "," names) "/.json"))
      (parse-response)))


(defn api-search-subreddits [credentials query]
  (-> (http-post credentials "https://oauth.reddit.com/api/search_subreddits"
                 {:form-params {:query query}})))



(defn api-recommend-subreddits [credentials srnames]
  (->> (http-get credentials (str "https://oauth.reddit.com/api/recommend/sr/"
                                  (apply str (interpose "," srnames))))
       (map :sr_name)))



(defn init
  [credentials]
  (let [response (get-access-token credentials)]
    (-> credentials
        (assoc :access-token (:access_token response))
        (assoc :expires-in (+ (System/currentTimeMillis) (:expires_in response))))))
