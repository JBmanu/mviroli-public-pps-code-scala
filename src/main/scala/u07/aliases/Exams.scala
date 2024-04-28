package u07.aliases

trait Exams:
  // abstracted types
  type Student
  type Teacher
  type Course
  type Evaluation
  trait Session:
    def id: Int

  // defined types
  case class Exam(course: Course, session: Session, student: Student)
  type Registration = (Exam, Evaluation)

  // methods
  def register(registration: Registration): Unit
  def evaluation(student: Student, course: Course): Evaluation

abstract class AbstractExams extends Exams:
  private var registrations: Set[Registration] = Set()

  override def register(registration: Registration) =
    registrations = registrations + registration

  override def evaluation(student: Student, course: Course) =
    registrations
      .collect:
        case (Exam(`course`,call,`student`), eval) => (call.id,eval)
      .maxBy(_._1)._2

class BasicExamsImpl extends AbstractExams:
  override type Student = String
  override type Teacher = String
  override type Course = String
  override type Evaluation = Int

  case class SessionWithId(id: Int) extends Session
  def session(id: Int): Session = SessionWithId(id)

object WorkWithExams extends App:

  val exams = new BasicExamsImpl()
  exams.register((exams.Exam("oop",exams.session(0),"mirko"),20))
  exams.register((exams.Exam("oop",exams.session(1),"mirko"),25))
  exams.register((exams.Exam("sisop",exams.session(0),"mirko"),30))

  println(exams.evaluation("mirko","oop")) // 25
  println(exams.evaluation("mirko","sisop")) // 30
