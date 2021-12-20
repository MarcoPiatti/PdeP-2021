import usuarios.*
import vehiculos.*
import controles.*

class Zona {
    const velocidadMaxima
    const usuariosEnTransito = #{}
    const controles = []

    method activarControles(){
        controles.forEach({ 
            unControl => usuariosEnTransito.forEach({ 
                unUsuario => unControl.controlar(unUsuario, velocidadMaxima)
            }) 
        })
    }
}

