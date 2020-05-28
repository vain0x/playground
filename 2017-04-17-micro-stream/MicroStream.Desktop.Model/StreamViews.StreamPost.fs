namespace MicroStream.StreamViews

open System
open MicroStream

/// Represents a post in a stream view.
type StreamPost(post: Post) =
  member this.CreatedAt = post.CreatedAt
  member this.DisplayName = post.Publisher.DisplayName
  member this.UserName = post.Publisher.UserName

  member this.Content =
    post.Content
      .Replace("<br>", Environment.NewLine)
      .Replace("<p>", "")
      .Replace("</p>", "")
