package io.higherkindness.ersatz

import java.util.UUID

final case class User(id: UUID)

final case class Tweet(id: UUID, text: String, user: User, replyTo: Tweet)

final case class Timeline(tweets: List[Tweet])