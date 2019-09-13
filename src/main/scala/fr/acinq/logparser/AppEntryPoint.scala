package fr.acinq.logparser

import java.nio.file.Paths

import scala.io.Source

object AppEntryPoint {

  val dateTimeRegex = "(\\d{4})-(\\d{2})-(\\d{2})\\s(\\d{2}):(\\d{2}):(\\d{2}),(\\d{3})".r
  val nodeAndChannelRegex = "n:([0-9a-fA-F]+) c:([0-9a-fA-F]+)".r
  val directionRegex = "\\s(IN|OUT)+\\s".r
  val lnMessageRegex = ("msg=(Ping|Pong|ChannelReestablish|OpenChannel|AcceptChannel|FundingCreated|FundingSigned|" +
    "FundingLocked|Shutdown|ClosingSigned|UpdateAddHtlc|UpdateFulfillHtlc|UpdateFailHtlc|UpdateFailMalformedHtlc|CommitSig|RevokeAndAck|" +
    "UpdateFee|AnnouncementSignatures|ChannelAnnouncement|Error)").r

  def main(args: Array[String]): Unit = {
    val (channelId, logFile) = args.toList match {
      case channel :: filePath :: Nil => (channel, Paths.get(filePath))
      case _ => throw new IllegalArgumentException("usage: log-parser <channelId> <path_to_log_file>")
    }

    println("#################################################################################")
    println(s"searching for $channelId in ${logFile.toString}")
    println(s"                    LOCAL <--------------------------------------------> REMOTE")

    Source.fromFile(logFile.toFile).getLines().foreach { line =>
      val dateTime = dateTimeRegex.findFirstIn(line)
      val nodeAndChannel = nodeAndChannelRegex.findFirstIn(line)
      val direction = directionRegex.findFirstIn(line).map(_.trim)
      val lnMessage = lnMessageRegex.findFirstIn(line).map(_.drop(4))

      val fee_opt = lnMessage match {
        case Some("UpdateFee") => extractFee(line)
        case _ => None
      }

      (dateTime, nodeAndChannel, direction, lnMessage) match {
        case (Some(date), Some(nc), Some(dir), Some(msg)) if nc.contains(channelId) =>
          println(prepareLine(Some(date.take(19)), dir, msg, fee_opt))
        case _ =>
      }
    }


  }

  def prepareLine(date: Option[String], direction: String, lnMessage: String, extra: Option[String]): String = {
    direction match {
      case "IN" =>  s"${date.getOrElse("")} <------------------ ${fixedSizeLnMessage(lnMessage)} ------------------- ${extra.getOrElse("")}"
      case "OUT" => s"${date.getOrElse("")} ------------------- ${fixedSizeLnMessage(lnMessage)} ------------------> ${extra.getOrElse("")}"
    }
  }

  def fixedSizeLnMessage(msg: String) = {
    val filler = (0 to 18 - msg.size).map(_ => " ").mkString // the biggest LN message is 18 when printed
    msg + filler
  }

  def extractFee(line: String): Option[String] = {
    //UpdateFee(5793e464cc8188d31dfba12d1a8fcd43f44e96b8b152d2d153e8e36ff4ce14e2,12314)
    val feerateRegex = "(,[0-9]+)".r
    feerateRegex.findFirstIn(line).map(fee => s"feerate=${fee.drop(1)}")
  }

}
