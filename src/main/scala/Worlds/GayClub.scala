package Worlds

import TruthEngine.Language._
import TruthEngine._
import TruthEngine.Truth.Truth

object GayClub extends GayClub

trait GayClub extends World[GayClub] {
  val worldInstance:GayClub = GayClub
  val name: String = "GayClub"
  val description:String = "They say there are no rules for love and taste... Well let's find out. Sober Customers speak the truth, Jerk Customers lie and Drunk Customers can say anything. They can have a crush or not in people in the Club and so for some songs, but that only happens during the conversations, before they don't. Everyone can hear the song played now but only DJ knows about future songs, other people can't even speak of the playlist but the DJ is so good that every song people speaks about is in the playlist. Workers as DJ and Waiter can only speak the truth, they like all the songs and they can't have crushes on people"

  //The only difference for the world is each list in the songlist
  def possibleWorldStates(war:Option[WorldAspectReference[GayClub, WorldState[GayClub]]], conversation:List[Sentence] = List()):List[WorldState[GayClub]] =
    war match {
      case _ =>
        getSongs(conversation)
    }

  //A world aspect is the song number n in the playlist
  def possibleWorldAspects(ws:Option[WorldState[GayClub]], conversation:List[Sentence] = List()):List[WorldAspectReference[GayClub, WorldState[GayClub]]] =
    ws match {
      case _ =>
        getStates(conversation)
    }

  def races(text:List[Sentence]):List[Race] = {
    val sexualPreferences:List[Option[SexualPreference]] = List(Some(Heterosexual), Some(Gay), Some(Bisexual), Some(Asexual))
    val genders:List[Option[Gender]] = List(Some(Boy), Some(Girl))
    val moods:List[Option[Mood]] = List(Some(Sober), Some(Drunk), Some(Jerk))

    val customers =
      for{
        sexualPreference <- sexualPreferences
        gender <- genders
        mood <- moods
        peopleCrush <- Some(None) :: getCharacters(text).map(char => Some(Some(char)))
        songCrush <- Some(None) :: getSongs(text).map(char => Some(Some(char)))
      } yield Customer(gender, sexualPreference, mood, peopleCrush, songCrush)

    customers ++ List(Worker(Some(DJ)), Worker(Some(Waiter)))
  }

  def checkConsistency(truth: Truth):Boolean = {
    val tupleTruth = truth.map(tp => (tp.reference, tp.state))
    val songs = tupleTruth.collect{
        case (_:SongReference, song) =>
          song
      }

    val peopleWhoLikeHerHimself = tupleTruth.collect {
        case (customerName, Some(Customer(_, _, _, Some(Some(personCrush)), _))) if customerName == personCrush =>
          customerName
      }

    val thereIsPeopleWhoLikesWrongSexualPreferences = tupleTruth.collect {
      case (customerName, Some(Customer(_, _, _, Some(Some(personCrush)), _))) =>
        for{
          likingPerson <- truth.find(_.reference == customerName)
          likingCustomer <- likingPerson.state match { case Some(likingCustomer:Customer) => Some(likingCustomer) case _ => None}
          personCrush <- truth.find(_.reference == personCrush)
          customerCrush <- personCrush.state match { case Some(customerCrush:Customer) => Some(customerCrush) case _ => None}
          crushGender <- customerCrush.gender
          checkedPreference <- checkPreferences(likingCustomer, crushGender)
        } yield checkedPreference
    }.flatten.exists(!_)

    val thereIsPeopleWhoLikesWorkers = tupleTruth.collect {
      case (_, Some(Customer(_, _, _, Some(Some(personCrush)), _))) =>
        for{
          personCrush <- truth.find(_.reference == personCrush)
          _ <- personCrush.state match { case Some(workerCrush:Worker) => Some(workerCrush) case _ => None}
        } yield false
    }.flatten.exists(!_)

    !songs.filter(!_.contains(UnknownSong)).exists(s => songs.count(_ == s) > 1) &&
      peopleWhoLikeHerHimself.isEmpty &&
      !thereIsPeopleWhoLikesWrongSexualPreferences &&
      !thereIsPeopleWhoLikesWorkers
  }

  sealed trait SongTrait extends WorldState[GayClub]
  trait SongReferenceTrait extends WorldAspectReference[GayClub, Song]
  case class SongReference(position:Int) extends SongReferenceTrait

  case object UnknownSong extends SongTrait {
    val stringRef:String = s"Unknown song"
    val description:String = "Only the DJs know which song it is"
  }
  case class Song(group:String, songName:String) extends SongTrait {
    val stringRef:String = s"$songName by $group"
    val description:String = "The DJs like this song"
  }

  sealed trait SexualPreference
  case object Heterosexual extends SexualPreference
  case object Gay extends SexualPreference
  case object Bisexual extends SexualPreference
  case object Asexual extends SexualPreference

  sealed trait Gender
  case object Girl extends Gender with Reference[State] {
    override def toString: String = "Girl"
  }
  case object Boy extends Gender with Reference[State] {
    override def toString: String = "Boy"
  }

  sealed trait Mood
  case object Sober extends Mood {
      override def toString: String = "Sober"
    }
  case object Drunk extends Mood {
    override def toString: String = "Drunk"
  }
  case object Jerk extends Mood {
    override def toString: String = "Jerk"
  }

  override def customMerge(tp1:TruthPiece[State], tp2:TruthPiece[State]): Option[TruthPiece[State]] =
    (tp1, tp2) match {
      case (Character(reference1, Some(Customer(gender1, sexualPreference1, mood1, personCrush1, songCrush1))), Character(reference2, Some(Customer(gender2, sexualPreference2, mood2, personCrush2, songCrush2)))) if reference1 == reference2 =>
        Some(Character(reference1, Some(
          Customer(
            mergeFeature(gender1, gender2),
            mergeFeature(sexualPreference1, sexualPreference2),
            mergeFeature(mood1, mood2),
            mergeFeature(personCrush1, personCrush2),
            mergeFeature(songCrush1, songCrush2)
          ))))
      case (Character(reference1, Some(Worker(job1))), Character(reference2, Some(Worker(job2)))) if reference1 == reference2 =>
        Some(Character(reference1, Some(Worker(mergeFeature(job1, job2)))))
      case _ =>
        None
    }


  trait PersonInGayClub extends Race {
    def description(capital:Boolean):String
    def canSay(truth: Truth, text:List[Sentence], sentenceIndex:Int):Boolean => Boolean =
      if(forbiddenSentence(truth, this, text(sentenceIndex))) _ => false
      else canSayAfterRestriction(truth, text, sentenceIndex)

    protected def canSayAfterRestriction(truth: Truth, text:List[Sentence], sentenceIndex:Int):Boolean => Boolean

    private def forbiddenSentence(truth:Truth, pigc:PersonInGayClub, sentence: Sentence):Boolean =
      (pigc, sentence) match {
        case (Worker(job), Sentence(_, _, _, ReferenceDirectObject(SongReference(index)), _)) if job.isEmpty || job.contains(DJ) || index == 0 =>
          false //If is the DJ, or maybe the DJ or is the current song, is ok i.e. forbiddenSentence = false
        case (_:Customer, Sentence(_, _, _, ReferenceDirectObject(SongReference(index)), _)) if index == 0 =>
          false //If is customer speaking about the current song, is ok i.e. forbiddenSentence = false
        case (_, Sentence(_, _, _, ReferenceDirectObject(_: SongReference), _)) =>
          true //otherwise they cannot speak about the song
        case (Worker(job), Sentence(_, SongReference(index), _, _, _)) if job.isEmpty || job.contains(DJ) || index == 0 =>
          false //If is the DJ, or maybe the DJ or is the current song, is ok i.e. forbiddenSentence = false
        case (_:Customer, Sentence(_, SongReference(index), _, _, _)) if index == 0 =>
          false //If is customer speaking about the current song, is ok i.e. forbiddenSentence = false
        case (_, Sentence(_, _: SongReference, _, _, _)) =>
          true //otherwise they cannot speak about the song
        case (_, Sentence(_, subject, Some(Like), ReferenceDirectObject(likedPerson: Name), _)) =>
          truth.find(_.reference == subject).fold(true) {
            case Character(_, Some(_: Worker)) =>
              true //No one can say a worker likes someone
            case _ =>
              subject == likedPerson //If someone likes her/himself we cannot say that
          }
        case _ =>
          false
      }

    protected def compareFeature[F](thisFeature:Option[F], otherFeature:Option[F]):Boolean =
      (thisFeature, otherFeature) match {
        case (Some(f), Some(of)) =>
          f == of
        case _ =>
          true
      }
    }

  case class Customer(gender:Option[Gender], sexualPreference:Option[SexualPreference], mood:Option[Mood], personCrush:Option[Option[Name]], songCrush:Option[Option[SongTrait]]) extends PersonInGayClub {
    val stringRef:String = "Customer"
    val description:String = description(capital = true)

    override def compare(other:State):Boolean =
      other match {
        case otherCustomer:Customer =>
          compareFeature(gender, otherCustomer.gender) &&
            compareFeature(sexualPreference, otherCustomer.sexualPreference) &&
            compareFeature(mood, otherCustomer.mood) &&
            compareFeature(personCrush, otherCustomer.personCrush) &&
            compareFeature(songCrush, otherCustomer.songCrush)
        case _ =>
          false
      }

    protected def canSayAfterRestriction(truth: Truth, text:List[Sentence], sentenceIndex:Int):Boolean => Boolean =
      mood match {
        case Some(Sober) =>
          b => b
        case Some(Jerk) =>
          b => !b
        case _ =>
          _ => true
      }

    def description(capital:Boolean):String = {
      val identityDescription =
        List(
          sexualPreference.map(_.toString),
          gender.map(_.toString),
          mood.map(_.toString)
        ).flatten
      val tasteDescription =
        List(
          personCrushPrinter(),
          songCrushPrinter()
        ).flatten

      (if(capital) "I" else "i") +
        "s a " +
        identityDescription.mkString(", ") +
        (if(identityDescription.isEmpty) "person" else "") +
        (if(tasteDescription.isEmpty) "" else " who ") +
        tasteDescription.mkString(" and ")
    }

    private def personCrushPrinter():Option[String] =
      personCrush.map{
        case Some(Name(crushName)) =>
          s"has a crush on $crushName"
        case None =>
          "doesn't have a crush"
      }

    private def songCrushPrinter():Option[String] =
      songCrush.map{
        case Some(Song(group, songName)) =>
          s"likes $songName by $group"
        case Some(UnknownSong) =>
          s"likes some song but we don't know which!"
        case None =>
          "doesn't like any song in particular"
      }
  }

  trait Job
  case object Waiter extends Job{
    override def toString: String = "Waiter"
  }
  case object DJ extends Job{
    override def toString: String = "DJ"
  }

  case class Worker(job:Option[Job]) extends PersonInGayClub {
    val stringRef:String = "Worker"
    val description:String = description(capital = true)

    override def compare(other:State):Boolean =
      other match {
        case otherWorker:Worker =>
          compareFeature(job, otherWorker.job)
        case _ =>
          false
      }

    protected def canSayAfterRestriction(truth: Truth, text:List[Sentence], sentenceIndex:Int):Boolean => Boolean =
      b => b

    def description(capital:Boolean):String = {
      (if(capital) "I" else "i") +
        "s a " +
        job.fold("Worker")(_.toString)
    }
  }

  case object Like extends NonCopulativeVerb {
    def truthPieceTrue(sentence:Sentence, truth:Truth, truthPieceIndex:Int):Option[Boolean] =
      sentence.directObject match {
        case StateDirectObject(song:Song) =>
          truth(truthPieceIndex).state.flatMap {
            case Customer(_, _, _, _, Some(Some(crushSong))) if sentence.directObjectAffirmation =>
              Some(crushSong == song)
            case Customer(_, _, _, _, Some(maybeCrushSong)) if !sentence.directObjectAffirmation =>
              Some(!maybeCrushSong.contains(song))
            case Customer(_, _, _, _, None) =>
              None
            case _:Worker =>
              Some(sentence.directObjectAffirmation)
            case _ =>
              Some(false)
          }
        case StateDirectObject(_) =>
          Some(false)
        case ReferenceDirectObject(SongReference(index)) =>
          (truth.find(tp => tp.reference == sentence.speaker).flatMap(_.state), truth(truthPieceIndex).state) match {
            case (identity, _) if identity.contains(Worker(Some(DJ))) || index == 0 =>
              truth.find(tp => tp.reference == SongReference(index)).map(_.state).flatMap {
                case Some(song: Song) =>
                  Sentence(sentence.speaker, sentence.subject, Some(Like), StateDirectObject(song), sentence.directObjectAffirmation).compareWithTruth(truth)
                case _ =>
                  None
              }
            case (identity, _) if identity.contains(Worker(None)) || identity.isEmpty =>
              None
            case _ =>
              Some(false)
          }
        case ReferenceDirectObject(gender:Gender) =>
          truth(truthPieceIndex).state match {
            case Some(customer: Customer) if sentence.directObjectAffirmation =>
              checkPreferences(customer, gender)
            case Some(customer: Customer) if !sentence.directObjectAffirmation =>
              checkPreferences(customer, gender).map(!_)
            case Some(_: Worker) => //Nobody can like workers
              Some(!sentence.directObjectAffirmation)
            case _ =>
              None
          }
        case ReferenceDirectObject(directObject) =>
          (truth(truthPieceIndex).state, sentence.subject) match {
            case (_, subject) if subject == directObject =>
              Some(false)
            case (Some(Customer(_, _, _, Some(Some(crushName)), _)), _) if sentence.directObjectAffirmation =>
              Some(crushName == directObject)
            case (Some(Customer(_, _, _, Some(maybeCrushName), _)), _) if !sentence.directObjectAffirmation =>
              Some(!maybeCrushName.contains(directObject))
            case (Some(Customer(_, _, _, None, _)), _) =>
              None
            case _ =>
              Some(false)
          }
      }
  }

  override lazy val  customParsers: List[LineParser[GayClub]] = List(GayClubLikeParser, GayClubDJParser, GayClubPersonParser)

  case object GayClubLikeParser extends LineParser[GayClub] {
    val forbiddenNames: List[String] = List("i", "someone", "everyone", "no one", "there", "are", "exactly", "am", "not", "like", "likes", "don't", "doesn't", "by", "boy", "boys", "girl", "girls")
    val world: GayClub = worldInstance
    val parserName: String = "GayClubParser"

    def translate(raw_script_sentence: String): Translation[String, Sentence] = {
      val sentenceRegex = """(\w+): (I |Someone |No one |There (is|are) (at least|at most|exactly) \d+ who |\w+ )(like|don't like|likes|doesn't like) (.+)""".r
      raw_script_sentence match {
        case sentenceRegex(speaker, raw_subject, _, _, raw_maybe_not, raw_direct_object) =>
          for {
            _ <- NotTranslated[String, Sentence](raw_script_sentence)
            subject <- parseSubject(speaker, raw_subject).newTranslation[Sentence]
            directObject <- parseDirectObject(raw_direct_object).newTranslation[Sentence]
            directObjectAffirmation <- parseDirectObjectAffirmation(raw_maybe_not)
          } yield Sentence(Name(speaker), subject, Some(Like), directObject, directObjectAffirmation)
        case _ =>
          NotTranslated(raw_script_sentence)
      }
    }

    private def parseSubject(speaker:String, raw_subject:String):Translation[String, Reference[State]] = {
      val atLeastRegex = """(There are at least|There is at least) (\d+) who """.r
      val atMostRegex = """(There are at most|There is at most) (\d+) who """.r
      val exactlyRegex = """(There are exactly|There is exactly) (\d+) who """.r
      val nameRegex = """(\w+) """.r

      raw_subject match {
        case "I " =>
          Translated(Name(speaker))
        case "Someone " =>
          Translated(NumberOfPeople(1, MoreOrEqual))
        case "Everyone " =>
          Translated(Everyone)
        case "No one " =>
          Translated(NumberOfPeople(0, Exactly))
        case atLeastRegex(_, n) =>
          Translated(NumberOfPeople(n.toInt, MoreOrEqual))
        case atMostRegex(_, n) =>
          Translated(NumberOfPeople(n.toInt, LessOrEqual))
        case exactlyRegex(_, n) =>
          Translated(NumberOfPeople(n.toInt, Exactly))
        case nameRegex(_name) =>
          Translated(Name(_name))
        case _ =>
          TranslationError(raw_subject, "GayClubParser thought it could translate this subject but failed")
      }
    }

    private def parseDirectObject(raw_direct_object:String): Translation[String, DirectObject] = {
      val songRegex = """(.+) by (.+)""".r
      val songReferenceRegex = """(this song|next song|the song in (\d+) songs)""".r

      raw_direct_object match {
        case songRegex(songTitle, songGroup) =>
          Translated(StateDirectObject(Song(songGroup, songTitle)))
        case songReferenceRegex(text, number) =>
          text match {
            case "this song" =>
              Translated(ReferenceDirectObject(SongReference(0)))
            case "next song" =>
              Translated(ReferenceDirectObject(SongReference(1)))
            case _ if number.toInt < 1 =>
              TranslationError(raw_direct_object, "GayClubParser found this invalid direct object")
            case _ =>
              Translated(ReferenceDirectObject(SongReference(number.toInt)))
          }
        case "girls" =>
          Translated(ReferenceDirectObject(Girl))
        case "boys" =>
          Translated(ReferenceDirectObject(Boy))
        case charName:String =>
          Translated(ReferenceDirectObject(Name(charName)))
        case _ =>
          TranslationError(raw_direct_object, "GayClubParser thought it could translate this direct object but failed")
      }

    }
    private def parseDirectObjectAffirmation(raw_maybe_not:String):Translation[String, Boolean] =
      Translated(raw_maybe_not == "like" || raw_maybe_not == "likes")
  }

  case object GayClubDJParser extends LineParser[GayClub] {
    val forbiddenNames: List[String] = List("this", "next", "song", "in", "songs", "not", "is", "by")
    val world: GayClub = worldInstance
    val parserName: String = "GayClubParser"

    def translate(raw_script_sentence: String): Translation[String, Sentence] = {
      val sentenceRegex = """(\w+): (This song|Next song|The song in \d+ songs) (is not|is) (.+) by (.+)""".r
      raw_script_sentence match {
        case sentenceRegex(speaker, raw_subject, raw_maybe_not, raw_song_name, raw_group) =>
          for {
            _ <- NotTranslated[String, Sentence](raw_script_sentence)
            subject <- parseSubject(raw_subject).newTranslation[Sentence]
            directObject <- Translated(StateDirectObject(Song(raw_group, raw_song_name))).newTranslation[Sentence]
            directObjectAffirmation <- parseDirectObjectAffirmation(raw_maybe_not)
          } yield Sentence(Name(speaker), subject, None, directObject, directObjectAffirmation)
        case _ =>
          NotTranslated(raw_script_sentence)
      }
    }

    private def parseSubject(raw_subject_verb:String):Translation[String, Reference[State]] = {
      val playListRegex = """The song in (\d+) songs""".r
      raw_subject_verb match {
        case "This song" =>
          Translated(SongReference(0))
        case "Next song" =>
          Translated(SongReference(1))
        case playListRegex(track) if track.toInt > 1 =>
          Translated(SongReference(track.toInt))
        case playListRegex(_) =>
         TranslationError(raw_subject_verb, """GayClubDJParser claims that the pattern The song in \d+ songs, should contain a number > 1""")
        case _ =>
          TranslationError(raw_subject_verb, "GayClubDJParser thought it could translate this subject but failed")
      }
    }

    private def parseDirectObjectAffirmation(raw_maybe_not:String):Translation[String, Boolean] =
      Translated(raw_maybe_not == "is")
  }

  case object GayClubPersonParser extends LineParser[GayClub] {
    val forbiddenNames: List[String] = List("is", "not", "boy", "girl", "heterosexual", "gay", "bisexual", "asexual", "sober", "drunk", "jerk", "dj", "waiter", "boys", "girls", "heterosexuals", "gays", "bisexuals", "asexuals", "sobers", "drunks", "jerks", "djs", "waiters")
    val world: GayClub = worldInstance
    val parserName: String = "GayClubParser"

    def translate(raw_script_sentence: String): Translation[String, Sentence] = {
      val sentenceRegex = """(\w+): (I am|Someone is|No one is|There (is|are) (at least|at most|exactly) \d+|(\w+) is)( not | )(a boy|boy|boys|a girl|girl|girls|heterosexual|heterosexual|gay|gays|bisexual|bisexuals|asexual|asexuals|sober|sobers|drunk|drunks|a jerk|jerk|jerk|DJ|DJs|a waiter|waiter|waiters)""".r
      raw_script_sentence match {
        case sentenceRegex(speaker, raw_subject, _, _, _, raw_maybe_not, raw_directObject) =>
          for {
            _ <- NotTranslated[String, Sentence](raw_script_sentence)
            subject <- parseSubject(speaker, raw_subject).newTranslation[Sentence]
            directObject <- parseDirectObject(raw_directObject).newTranslation[Sentence]
            directObjectAffirmation <- parseDirectObjectAffirmation(raw_maybe_not)
          } yield Sentence(Name(speaker), subject, None, directObject, directObjectAffirmation)
        case _ =>
          NotTranslated(raw_script_sentence)
      }
    }

    private def parseSubject(speaker:String, raw_subject:String):Translation[String, Reference[State]] = {
      val atLeastRegex = """(There are at least|There is at least) (\d+)""".r
      val atMostRegex = """(There are at most|There is at most) (\d+)""".r
      val exactlyRegex = """(There are exactly|There is exactly) (\d+)""".r
      val nameRegex = """(\w+) is""".r

      raw_subject match {
        case "I am" =>
          Translated(Name(speaker))
        case "Someone is" =>
          Translated(NumberOfPeople(1, MoreOrEqual))
        case "Everyone is" =>
          Translated(Everyone)
        case "No one is" =>
          Translated(NumberOfPeople(0, Exactly))
        case atLeastRegex(_, n) =>
          Translated(NumberOfPeople(n.toInt, MoreOrEqual))
        case atMostRegex(_, n) =>
          Translated(NumberOfPeople(n.toInt, LessOrEqual))
        case exactlyRegex(_, n) =>
          Translated(NumberOfPeople(n.toInt, Exactly))
        case nameRegex(_name) =>
          Translated(Name(_name))
        case _ =>
          TranslationError(raw_subject, "GayClubPersonParser thought it could translate this subject but failed")
      }
    }

    private def parseDirectObject(raw_direct_object:String):Translation[String, DirectObject] =
      raw_direct_object match {
        case "a boy"|"boy"|"boys" =>
          Translated(StateDirectObject(Customer(Some(Boy), None, None, None, None)))
        case "a girl"|"girl"|"girls" =>
          Translated(StateDirectObject(Customer(Some(Girl), None, None, None, None)))
        case "heterosexual"|"heterosexuals" =>
          Translated(StateDirectObject(Customer(None, Some(Heterosexual), None, None, None)))
        case "gay"|"gays" =>
          Translated(StateDirectObject(Customer(None, Some(Gay), None, None, None)))
        case "bisexual"|"bisexuals" =>
          Translated(StateDirectObject(Customer(None, Some(Bisexual), None, None, None)))
        case "asexual"|"asexuals" =>
          Translated(StateDirectObject(Customer(None, Some(Asexual), None, None, None)))
        case "sober"|"sobers" =>
          Translated(StateDirectObject(Customer(None, None, Some(Sober), None, None)))
        case "drunk"|"drunks" =>
          Translated(StateDirectObject(Customer(None, None, Some(Drunk), None, None)))
        case "a jerk"|"jerks" =>
          Translated(StateDirectObject(Customer(None, None, Some(Jerk), None, None)))
        case "a DJ"|"DJ"|"DJs" =>
          Translated(StateDirectObject(Worker(Some(DJ))))
        case "a waiter"|"waiter"|"waiters" =>
          Translated(StateDirectObject(Worker(Some(Waiter))))
        case _ =>
          TranslationError(s"Sentence containing $raw_direct_object", s"GayClubPersonParser thought it could translate this sentence but failed")
      }

    private def parseDirectObjectAffirmation(raw_maybe_not:String):Translation[String, Boolean] =
      Translated(raw_maybe_not != " not ")
  }

  override lazy val  customPrinters: List[TruthPiecePrinter] = List(GayClubPrinter)

  case object GayClubPrinter extends TruthPiecePrinter {
    def translate(raw_script_sentence: TruthPiece[State]): Translation[TruthPiece[State], String] =
      (raw_script_sentence.reference, raw_script_sentence.state) match {
        case (Name(customerName), Some(pigc:PersonInGayClub)) =>
          Translated(s"$customerName ${pigc.description(capital = false)}")
        case (SongReference(index), Some(Song(group, songName))) =>
          val reference =
            if(index == 0) "The song playing now"
            else if(index == 1) "Next song"
            else s"The song number $index in the playlist"
          Translated(s"$reference is $songName by $group")
        case (SongReference(index), Some(UnknownSong)) =>
          val reference =
            if(index == 0) "The song playing now"
            else if(index == 1) "Next song"
            else s"The song number $index in the playlist"
          Translated(s"$reference is Unknown")
        case _ =>
          NotTranslated(raw_script_sentence)
      }
  }

  private def getCharacters(conversation:List[Sentence]):List[Name] =
    (conversation.collect{ case Sentence(speaker:Name, _, _, _, _) => speaker} ++
      conversation.collect{ case Sentence(_, personSubject:Name, _, _, _) => personSubject} ++
      conversation.collect{ case Sentence(_, _, _, ReferenceDirectObject(personDirectObject:Name), _) => personDirectObject}).distinct

  private def getSongs(conversation:List[Sentence]):List[SongTrait] =
    UnknownSong :: conversation.collect{ case Sentence(_, _, _, StateDirectObject(song:Song), _) => song}.distinct

  private def getStates(conversation:List[Sentence]):List[SongReferenceTrait] = {
    val referencedSongs = getSongs(conversation).indices.map(SongReference.apply).length
    val playlistPositions = conversation.collect {
      case Sentence(_, _, _, ReferenceDirectObject(SongReference(index)), _) =>
        index
      case Sentence(_, SongReference(index), _, _, _) =>
        index
    }
    val playListPositionsLength = if(playlistPositions.isEmpty) 0 else playlistPositions.max + 1
    (0 until List(referencedSongs, playListPositionsLength).max).map(SongReference.apply).toList
  }

  private def checkPreferences(customer:Customer, likedGender:Gender):Option[Boolean] =
    (customer.gender, customer.sexualPreference) match {
      case (Some(Girl), Some(Gay)) =>
        Some(likedGender == Girl)
      case (Some(Girl), Some(Heterosexual)) =>
        Some(likedGender == Boy)
      case (Some(Boy), Some(Gay)) =>
        Some(likedGender == Boy)
      case (Some(Boy), Some(Heterosexual)) =>
        Some(likedGender == Girl)
      case (_, Some(Bisexual)) =>
        Some(true)
      case (_, Some(Asexual)) =>
        Some(false)
      case _ =>
        None
    }

  private def mergeFeature[F](thisFeature:Option[F], otherFeature:Option[F]):Option[F] =
    (thisFeature, otherFeature) match {
      case (Some(f), Some(of)) if f == of =>
        Some(f)
      case _ =>
        None
    }
}
