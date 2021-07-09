use crate::cookbook::{Cookbook, Recipe};
use crate::genetics::*;
use crate::magic::{TurtlePower, World};
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug)]
pub struct Turtle {
    name: String,
    walking_speed: u32,
    favorite_color: Color,
    favorite_flavor: Flavor,
    children_count: i32,
    children: Vec<Rc<RefCell<Turtle>>>,
    magic: Option<Box<dyn TurtlePower>>,
}

pub struct NoMagicalItemError;

impl Turtle {
    /**
     * Returns an appropriately-initialized Turtle.
     */
    pub fn new(name: String, walking_speed: u32, favorite_color: Color, favorite_flavor: Flavor) -> Turtle {
        Turtle {name, walking_speed, favorite_color, favorite_flavor, children_count:0, children: Vec::new(), magic: None}
    }

    pub fn walking_speed(&self) -> u32 {
        self.walking_speed
    }

    pub fn favorite_flavor(&self) -> Flavor {
        self.favorite_flavor
    }

    pub fn name(&self) -> &String {
        &(self.name)
    }

    pub fn favorite_color(&self) -> &Color {
        &(self.favorite_color)
    }
    pub fn children(&mut self, turtle: Rc<RefCell<Turtle>>) {
        self.children.push(turtle)
    }

    /**
     * Returns Some of any recipe from the given cookbook that matches the turtle's flavor
     * preferences, or None if no such recipe exists.
     *
     * IMPORTANT: you will need to add lifetime parameters to this function. It is
     * up to you to figure out which ones and where. Do not make any other changes
     * to the signature.
     */
    pub fn choose_recipe<'a>(&self, cookbook: &'a Cookbook) -> Option<&'a Recipe> {
        let mut rec = None;
        {
            for i in cookbook.recipes() {
                if self.favorite_flavor == i.flavor() {
                    rec=Some(i)
                }
            }
        }rec
    }

    /**
     * This function should take ownership of the magical item and save it in a field of 'self' for future use.
     * If there is already a magical item, the new item should be saved (the old item can be discarded).
     */
    pub fn take_magical_item(&mut self, item: Box<dyn TurtlePower>) {
        self.magic = Some(item);
    }

    /**
     * If there is a saved magical item, this function should activate it and return Ok(()).
     * Otherwise, it should return the appropriate error.
     */
    pub fn activate_magical_item(&mut self, world: &mut World) -> Result<(), NoMagicalItemError> {
        /*let mut re = &self.magic;*/
        let r = match self.magic.take() {
            None => Err(NoMagicalItemError),
            Some(mut t) => Ok(t.activate(world)),
        };
        r
    }

    /**
     * Returns the number of children that this turtle has.
     */
    pub fn num_children(&self) -> usize {
        self.children_count as usize
    }
    pub fn children_count(&mut self) {
        self.children_count += 1;
    }

    /**
     * Gives every child walking lessons, which increases their walking speed by 1.
     * (You think walking is easy? Try doing it while carrying your house on your back!)
     */
    pub fn teach_children(&mut self) {
        for i in 0..self.children.len() {
            self.children[i].borrow_mut().walking_speed += 1; /*self.children[i].borrow_mut().walking_speed() + 1;*/
        }
    }
    pub fn breed(c1: &Color, c2: &Color, name: String) -> Turtle {
        let color = Color::cross(c1,c2);
        Turtle::new(name,1,color,Flavor::random_flavor())
    }
    /*pub fn breed(t1: Turtle, t2: Turtle, name: String) -> Turtle {
        let color = Color::new(t1.favorite_color(), t2.favorite_color());
        Turtle::new(name,1,color,Flavor::random_flavor())
    }*/
}
