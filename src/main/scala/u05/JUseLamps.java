package u05;

/**
 * Created by mirko on 12/7/16.
 */
public class JUseLamps {

    public static void main(String[] args) {

        // note, it works with scala Lamp, too!
        System.out.println("starting..");
        final JLamp v = new JLamp(false);
        System.out.println(v);
        System.out.println(v.isOn());
        v.switchOn();
        System.out.println(v.isOn());

        final JLamp v2 = new JLamp(true);
        System.out.println(v2.isOn());
        final JLamp v3 = new JLamp();
        System.out.println(v3.isOn());

    }
}
