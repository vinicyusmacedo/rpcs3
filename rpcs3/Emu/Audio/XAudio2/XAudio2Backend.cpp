#ifdef _WIN32

#include "Utilities/Log.h"
#include "Utilities/StrFmt.h"

#include "XAudio2Backend.h"
#include <Windows.h>

XAudio2Backend::XAudio2Backend()
{
	if (lib = LoadLibraryExW(L"XAudio2_9.dll", nullptr, LOAD_LIBRARY_SEARCH_SYSTEM32))
	{
		// xa28* implementation is fully compatible with library 2.9
		m_funcs.init       = &xa28_init;
		m_funcs.destroy    = &xa28_destroy;
		m_funcs.play       = &xa28_play;
		m_funcs.flush      = &xa28_flush;
		m_funcs.stop       = &xa28_stop;
		m_funcs.open       = &xa28_open;
		m_funcs.is_playing = &xa28_is_playing;
		m_funcs.add        = &xa28_add;
		m_funcs.enqueued_samples = &xa28_enqueued_samples;
		m_funcs.set_freq_ratio   = &xa28_set_freq_ratio;

		LOG_SUCCESS(GENERAL, "XAudio 2.9 found");
		return;
	}

	if (auto lib = LoadLibraryExW(L"XAudio2_8.dll", nullptr, LOAD_LIBRARY_SEARCH_SYSTEM32))
	{
		m_funcs.init       = &xa28_init;
		m_funcs.destroy    = &xa28_destroy;
		m_funcs.play       = &xa28_play;
		m_funcs.flush      = &xa28_flush;
		m_funcs.stop       = &xa28_stop;
		m_funcs.open       = &xa28_open;
		m_funcs.is_playing = &xa28_is_playing;
		m_funcs.add        = &xa28_add;
		m_funcs.enqueued_samples = &xa28_enqueued_samples;
		m_funcs.set_freq_ratio   = &xa28_set_freq_ratio;

		LOG_SUCCESS(GENERAL, "XAudio 2.8 found");
		return;
	}

	if (auto lib = LoadLibraryExW(L"XAudio2_7.dll", nullptr, LOAD_LIBRARY_SEARCH_SYSTEM32))
	{
		m_funcs.init       = &xa27_init;
		m_funcs.destroy    = &xa27_destroy;
		m_funcs.play       = &xa27_play;
		m_funcs.flush      = &xa27_flush;
		m_funcs.stop       = &xa27_stop;
		m_funcs.open       = &xa27_open;
		m_funcs.is_playing = &xa27_is_playing;
		m_funcs.add        = &xa27_add;
		m_funcs.enqueued_samples = &xa27_enqueued_samples;
		m_funcs.set_freq_ratio   = &xa27_set_freq_ratio;

		LOG_SUCCESS(GENERAL, "XAudio 2.7 found");
		return;
	}

	fmt::throw_exception("No supported XAudio2 library found");
}

XAudio2Backend::~XAudio2Backend()
{
	ASSERT(!initialized);
}

void XAudio2Backend::Play()
{
	m_funcs.play();
}

void XAudio2Backend::Close()
{
	m_funcs.stop();
	m_funcs.flush();

	if (initialized)
	{
		m_funcs.destroy();
		initialized = false;
	}
}

void XAudio2Backend::Pause()
{
	m_funcs.stop();
}

void XAudio2Backend::Open(u32 /* num_buffers */)
{
	if (!initialized)
	{
		ASSERT(lib != nullptr);
		m_funcs.init(lib);
		initialized = true;
		LOG_SUCCESS(GENERAL, "XAudio initialized");
	}

	m_funcs.open();
}

bool XAudio2Backend::IsPlaying()
{
	return m_funcs.is_playing();
}

bool XAudio2Backend::AddData(const void* src, u32 num_samples)
{
	return m_funcs.add(src, num_samples);
}

void XAudio2Backend::Flush()
{
	m_funcs.flush();
}

u64 XAudio2Backend::GetNumEnqueuedSamples()
{
	return m_funcs.enqueued_samples();
}

f32 XAudio2Backend::SetFrequencyRatio(f32 new_ratio)
{
	return m_funcs.set_freq_ratio(new_ratio);
}

#endif
