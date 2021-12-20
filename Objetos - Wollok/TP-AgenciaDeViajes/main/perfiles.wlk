import mediosDeTransporte.*
import viajes.*

class Perfil {
	method elegirViaje(viajesCandidatos, dinero) {
		return viajesCandidatos.min({ unViaje => unViaje.transporte().demora() })
	}
}

object perfilEmpresarial inherits Perfil { }

object perfilEstudiantil inherits Perfil {
	override method elegirViaje(viajesCandidatos, dinero) {
		return super(self.viajesCosteables(viajesCandidatos, dinero), dinero)
	}

	method viajesCosteables(viajesCandidatos, dinero) { // El presupuesto del estudiante decide el transporte, pero el presupuesto se gasta en todo el viaje
		return viajesCandidatos.filter({ unViaje => unViaje.costo() < dinero }) // Por lo cual creemos que es necesario pasarle todo un viaje entero para evaluar el costo
	}
}

object perfilGrupoFamiliar inherits Perfil {
	override method elegirViaje(viajesCandidatos, dinero){
		return viajesCandidatos.anyOne()
	}
}