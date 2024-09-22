#include "math.h"
#include "SDL.h"

namespace output
{
    struct RGBA32
    {
        uint8_t R;
        uint8_t G;
        uint8_t B;
        uint8_t A;
    };

    class window
    {
public:
        SDL_Window* handle;
        SDL_Renderer* renderer;
        SDL_Texture* frame;
        window(const char* title, uint x, uint y, uint width, uint height)
        {
            auto res = SDL_CreateWindowAndRenderer(width, height, 0, &handle, &renderer);
            if(res != 0)
            {
                std::cout << SDL_GetError();
                throw std::runtime_error("WINDOW ERROR");
            }
            frame = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_STREAMING, width, height);
        }

        template<int nLines, int lineSpan>
        void write_frame(math::matrix<nLines, lineSpan, RGBA32>& framedata)
        {
            void* texPtr;
            int texPitch;
            SDL_LockTexture(frame, nullptr, &texPtr, &texPitch);
            memcpy(texPtr, framedata.data.begin(), texPitch * framedata.rows());
            SDL_UnlockTexture(frame);

            SDL_RenderClear(renderer);
//            SDL_Rect destination{0, 0, framedata.cols(), framedata.rows()};
            SDL_RenderCopy(renderer, frame, nullptr, nullptr);
            SDL_RenderPresent(renderer);
        }
    };
};
