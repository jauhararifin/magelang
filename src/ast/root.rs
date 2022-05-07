use super::FnDeclNode;

#[derive(Debug, Clone)]
pub struct RootNode {
    pub declarations: Vec<DeclNode>,
}

#[derive(Debug, Clone)]
pub enum DeclNode {
    Fn(FnDeclNode),
}

impl DeclNode {
    pub fn is_func(&self) -> bool {
        self.try_unwrap_func().is_some()
    }

    pub fn try_unwrap_func(&self) -> Option<&FnDeclNode> {
        let Self::Fn(t) = self;
        Some(t)
    }

    pub fn unwrap_func(&self) -> &FnDeclNode {
        self.try_unwrap_func().unwrap()
    }
}
