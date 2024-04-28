package u07.contextual

import java.awt.event.*
import javax.swing.*

object ScalaSwingFacade:

  def createPanel(comps: JComponent*): JPanel =
    comps.foldLeft(new JPanel()): (p, c) => 
      p.add(c); p

  given Conversion[ActionEvent => Unit, ActionListener] = f => f(_)
    // f => new ActionListener(){ def actionPerformed(e: ActionEvent) = f(e) }
    // f => e => f(e)

  extension (b: JButton)
    def addActionListener(action: => Unit): Unit = addActionListener(e => action)

object TrySwing extends App:
  import ScalaSwingFacade.{*, given}

  val f = new JFrame("title")
  val text = new JTextField(10)
  val bok = new JButton("OK")
  val bevt = new JButton("Event")
  val bquit = new JButton("Quit")
  val p = createPanel(text, bok, bevt, bquit)
  bok.addActionListener(JOptionPane.showMessageDialog(f,s"Text: ${text.getText}"))
  bevt.addActionListener(JOptionPane.showMessageDialog(f, _))
  bquit.addActionListener(System.exit(0))
  f.getContentPane.add(p)
  f.pack()
  f.setVisible(true)