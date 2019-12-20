package MyExceptions

final case class NegativeKeyException(
										 private val message: String = "",
										 private val cause: Throwable = None.orNull
									 ) extends Exception(message, cause)

final case class Segmentfault(
							 private val message: String = "",
							 private val cause: Throwable = None.orNull
							 ) extends Exception(message, cause)

final case class AnywayThisIsAnException(
										 private val message: String = "",
										 private val cause: Throwable = None.orNull
										 ) extends Exception(message, cause)