use crate::turtle::Turtle;
use std::rc::Rc;
use std::cell::RefCell;
use std::cell::RefMut;
use std::collections::HashMap;
use std::ops::DerefMut;
use std::ops::Deref;

/**
 * TurtleRef describes a reference to a turtle that is owned by
 * the campus. These references may need to be passed around outside
 * Campus (in this case, the test harness needs access to them).
 * We could have specified the type precisely, but that would expose
 * implementation details of Campus to the clients.
 *
 * To avoid doing that, TurtleRef serves as an intermediary whenever
 * a reference to a Turtle needs to escape Campus.
 *
 * You will need to fill in TurtleRef with a field that reflects
 * your implementation. In order to manage lifetimes correctly,
 * TurtleRef does not provide access to a &Turtle; it instead
 * supplies a BorrowedTurtle, which implements Deref.
 * 
 * Another way of thinking about this is that TurtleRef provides 
 * a persistent reference, whereas a BorrowedTurtle is ephemeral,
 * generated temporarily while the Turtle is needed.
 *
 * An example use:
 *
 * let turtle_ref: TurtleRef = campus.get_turtle(...);
 * let turtle_name: &String = turtle_ref.borrowed_turtle().name();
 *
 * This code is here because the code below it relies on it. However, we suggest
 * implementing this AFTER you have figured out how to represent Campus, not before.
 */
//#[derive(Debug)]
pub struct TurtleRef {
    bt: Rc<RefCell<Turtle>>,
}

impl TurtleRef {
    pub fn new(t: Rc<RefCell<Turtle>>) -> TurtleRef {
        TurtleRef{bt: t}
    }
    pub fn borrowed_turtle(&self) -> BorrowedTurtle {
        BorrowedTurtle {r: self.bt.borrow_mut()}
    }
}    

// Hint: BorrowedTurtle might need a lifetime parameter.
pub struct BorrowedTurtle<'a> {
    r: RefMut<'a,Turtle>,
}

// Hint: update this signature when you figure out the lifetime for BorrowedTurtle.
impl<'a> std::ops::Deref for BorrowedTurtle<'a> {
    type Target = Turtle;

    fn deref(&self) -> &Self::Target {
        & *self.r    
    }
}

// Hint: update this signature when you figure out the lifetime for BorrowedTurtle.
impl<'a> std::ops::DerefMut for BorrowedTurtle<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut *self.r
    }
}

//#[derive(Debug)]
pub struct Campus {
    t: Vec<TurtleRef>,
    size: i32,
    h: RefCell<HashMap<String,Rc<Vec<TurtleRef>>>>,
    hold: RefCell<HashMap<String,String>>,
}

impl Campus {
    pub fn new() -> Campus {
        Campus {t: Vec::new(), size: 0, h: RefCell::new(HashMap::new()), hold: RefCell::new(HashMap::new())}
    }

    /**
     * Returns the number of turtles present.
     */
    pub fn size(&self) -> usize {
        self.size as usize
    }

    /**
     * This moves 'turtle', taking ownership. Do NOT implement Copy for Turtle.
     *
     * After add_turtle returns, the Campus should hold the turtle in its collection of turtles. The new turtle should be at the END of the collection.
     */
    pub fn add_turtle(&mut self, turtle: Turtle) {
        self.t.push(TurtleRef::new(Rc::new(RefCell::new(turtle))));
        self.size += 1;
        /*for (key, value) in Rc::get_mut(self.h.borrow_mut().unwrap()) {
            println!("{} / {}", key, value.len());
        }*/
        let rf = &self.get_turtle((self.size - 1) as usize).borrowed_turtle().name().clone();
        if self.hold.borrow_mut().contains_key(&self.get_turtle((self.size - 1) as usize).borrowed_turtle().name().clone()) {
            if self.hold.borrow_mut().get(rf).unwrap() == "a" {
                //Rc::get_mut(&mut self.h.get(rf).unwrap().clone()).unwrap().push(self.get_turtle((self.size - 1) as usize));
                //println!("h is {:?}", Rc::get_mut(&mut self.h.borrow_mut().get_mut(rf).unwrap()));
                Rc::get_mut(&mut self.h.borrow_mut().get_mut(rf).unwrap()).unwrap().push(self.get_turtle((self.size - 1) as usize));
            }
        } else {
            self.hold.borrow_mut().insert(self.get_turtle((self.size - 1) as usize).borrowed_turtle().name().clone(),"b".to_string());
        }
    }

    /**
     * Gets a reference to a turtle at an index. Panics if index is out of bounds.
     */
    pub fn get_turtle(&self, index: usize) -> TurtleRef {
        TurtleRef::new(self.t[index].bt.clone())
    }

    /**
     * Breeds the turtles at the given turtle indices, adding the new turtle to the end of the turtle vector.
     * If the indices are out of bounds, the method should panic.
     *
     * Should probably call a method you add to Turtle, which itself
     * uses the various 'cross' methods on the fields to initialize the new Turtle.
     * The new turtle should have a walking speed of 1 (babies go slowly).    
     * The new turtle should have its new favorite flavor selected randomly, 
     * and the new favorite color should be the result of crossing the 
     * parents' favorite colors.
     */
    pub fn breed_turtles(&mut self, t1_index: usize, t2_index: usize, child_name: String) {
        let t = Rc::new(RefCell::new(Turtle::breed(self.get_turtle(t1_index).borrowed_turtle().favorite_color(), self.get_turtle(t2_index).borrowed_turtle().favorite_color(), child_name)));
        let m = t.clone();
        let d = t.clone();
        self.t.push(TurtleRef::new(t));
        self.size += 1;
        DerefMut::deref_mut(&mut self.get_turtle(t1_index).borrowed_turtle()).children(m);
        DerefMut::deref_mut(&mut self.get_turtle(t2_index).borrowed_turtle()).children(d);
        DerefMut::deref_mut(&mut self.get_turtle(t1_index).borrowed_turtle()).children_count();
        DerefMut::deref_mut(&mut self.get_turtle(t2_index).borrowed_turtle()).children_count();
    }

    /**
     * Returns None if the campus is empty. Otherwise, returns Some of a reference to the turtle with the fastest walking speed.
     */
    pub fn fastest_walker(&self) -> Option<TurtleRef> {
        let mut fast = None;
        let mut min = 0;
        for i in 0..self.t.len() {
            if self.size == 1 || self.get_turtle(i).borrowed_turtle().walking_speed() > min {
                fast = Some(self.get_turtle(i));
                min = self.get_turtle(i).borrowed_turtle().walking_speed();
            }
        } 
        fast
    }

    /**
     * Implement this for "Finding Testudo".
     * This interface will NOT work for "Fast Turtle Lookup".
     */
    pub fn turtles_with_name(&self, name: &str) -> Vec<TurtleRef> {
        let mut tur = Vec::new();
        for i in 0..self.t.len() {
            if self.get_turtle(i).borrowed_turtle().name() == name {
                tur.push(self.get_turtle(i));
            }
        }
        tur
    }

    pub fn turtles_with_name_cached(&self, name: &str) -> Rc<Vec<TurtleRef>> {
        if self.hold.borrow_mut().contains_key(name) {
            if self.hold.borrow_mut().get(name).unwrap() == "b"{
                self.hold.borrow_mut().entry(name.to_string()).and_modify(|x| {*x = "a".to_string()});
                self.h.borrow_mut().insert(name.to_string(), Rc::new(self.turtles_with_name(name)));
                //println!("it is {:?}", self.h);
                Rc::new(self.turtles_with_name(name))
            } else {
                self.h.borrow_mut().get(name).unwrap().clone()
            }
        } else {
            Rc::new(Vec::new())
        }
    }
}
